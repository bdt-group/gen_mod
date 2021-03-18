%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(gen_mod).

%% API
-export([start/0, stop/0]).
-export([load/1, load/2]).
-export([unload/0, unload/1]).
-export([start_server/1, start_server/2, stop_server/1]).
-export([start_statem/1, start_statem/2, stop_statem/1]).
-export([get_opt/2, get_opt/3]).
-export([is_loaded/1, is_loaded/2]).
-export([loaded/0, loaded/1]).
-export_type([scope/0, option/0, options/0]).
-export_type([load_error/0]).

-include_lib("kernel/include/logger.hrl").

-type option() :: atom().
-type options() :: #{option() => term()}.
-type scope() :: term().

-type load_error() ::
        {missing_required_module_option, module(), option()} |
        {unknown_module_option, module(), option()} |
        {missing_module_dep, module(), module()} |
        {failed_modules, [module()]}.

-callback load(options()) -> ok | {ok, options()} | {ok, pid(), options()}.
-callback unload(options()) -> any().
-callback reload(options(), options()) -> ok | {ok, options()}.
-callback defaults() -> options().
-callback required() -> [option()].
-callback depends(options()) -> [module()].

-optional_callbacks([unload/1, reload/2, defaults/0, required/0, depends/1]).

%%%===================================================================
%%% API
%%%===================================================================
-spec start_server(module()) -> {ok, pid()} | {error, {already_started, pid()} | term()}.
start_server(Mod) ->
    start(gen_server, Mod, Mod).

-spec start_server(module(), atom()) -> {ok, pid()} | {error, {already_started, pid()} | term()}.
start_server(Mod, Proc) ->
    start(gen_server, Mod, Proc).

-spec start_statem(module()) -> {ok, pid()} | {error, {already_started, pid()} | term()}.
start_statem(Mod) ->
    start(gen_statem, Mod, Mod).

-spec start_statem(module(), atom()) -> {ok, pid()} | {error, {already_started, pid()} | term()}.
start_statem(Mod, Proc) ->
    start(gen_statem, Mod, Proc).

-spec stop_server(atom()) -> ok | {error, term()}.
stop_server(Proc) ->
    stop(Proc).

-spec stop_statem(atom()) -> ok | {error, term()}.
stop_statem(Proc) ->
    stop(Proc).

-spec load([{module(), options()}]) -> ok | {error, load_error()}.
load(ModOpts) ->
    load(ModOpts, undefined).

-spec load([{module(), options()}], scope()) -> ok | {error, load_error()}.
load(ModOpts, Scope) ->
    try sort_modules(lists:map(fun merge_opts/1, ModOpts)) of
        ModOpts1 ->
            FailedMods = lists:filtermap(
                           fun({Mod, Opts, Order}) ->
                                   Res = case ets:lookup(modules, {Mod, Scope}) of
                                             [{_, OldOpts, _OldOrder}] ->
                                                 reload(Mod, Scope, Opts, OldOpts, Order);
                                             [] ->
                                                 load(Mod, Scope, Opts, Order)
                                         end,
                                   case Res of
                                       ok -> false;
                                       error -> {true, Mod}
                                   end
                           end, ModOpts1),
            lists:foreach(
              fun({Mod, Opts}) ->
                      case lists:keymember(Mod, 1, ModOpts1) of
                          true -> ok;
                          false -> unload(Mod, Scope, Opts)
                      end
              end, lists:reverse(loaded(Scope))),
            case FailedMods of
                [] -> ok;
                _ -> {error, {failed_modules, FailedMods}}
            end
    catch
        throw:{?MODULE, Error} ->
            {error, Error}
    end.

-spec unload() -> ok.
unload() ->
    unload(undefined).

-spec unload(scope()) -> ok.
unload(Scope) ->
    lists:foreach(
      fun({Mod, Opts}) ->
              unload(Mod, Scope, Opts)
      end, lists:reverse(loaded(Scope))).

-spec is_loaded(module()) -> boolean().
is_loaded(Mod) ->
    is_loaded(Mod, undefined).

-spec is_loaded(module(), scope()) -> boolean().
is_loaded(Mod, Scope) ->
    ets:member(modules, {Mod, Scope}).

-spec get_opt(option(), options() | module()) -> term().
get_opt(Opt, Opts) when is_map(Opts) ->
    maps:get(Opt, Opts);
get_opt(Opt, Mod) ->
    get_opt(Opt, Mod, undefined).

-spec get_opt(option(), module(), scope()) -> term().
get_opt(Opt, Mod, Scope) ->
    Opts = ets:lookup_element(modules, {Mod, Scope}, 2),
    maps:get(Opt, Opts).

-spec loaded() -> [{scope(), [{module(), options()}, ...]}].
loaded() ->
    maps:to_list(
      lists:foldr(
        fun({{Mod, Scope}, Opts, _Order}, Acc) ->
                maps:update_with(
                  Scope,
                  fun(ModOpts) -> [{Mod, Opts}|ModOpts] end,
                  [{Mod, Opts}], Acc)
        end, #{}, lists:keysort(3, ets:tab2list(modules)))).

-spec loaded(scope()) -> [{module(), options()}].
loaded(Scope) ->
    [{Mod, Opts} || {{Mod, S}, Opts, _Order} <- lists:keysort(3, ets:tab2list(modules)),
                    S == Scope].

-spec start() -> ok | {error, term()}.
start() ->
    case application:ensure_all_started(?MODULE) of
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end.

-spec stop() -> ok | {error, term()}.
stop() ->
    case application:stop(?MODULE) of
        ok -> ok;
        {error, {not_started, _}} -> ok;
        Err -> Err
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec load(module(), scope(), options(), pos_integer()) -> ok | error.
load(Mod, Scope, Opts, Order) ->
    try Mod:load(Opts) of
        ok ->
            do_load(Mod, Scope, Opts, Order);
        {ok, ModifiedOpts} when is_map(ModifiedOpts) ->
            do_load(Mod, Scope, ModifiedOpts, Order);
        {ok, Pid, ModifiedOpts} when is_pid(Pid), is_map(ModifiedOpts) ->
            do_load(Mod, Scope, ModifiedOpts, Order);
        Error ->
            ets:delete(modules, {Mod, Scope}),
            ?LOG_ERROR(
               "Unexpected return value from ~s:load/1:~n"
               "** Options = ~p~n"
               "** Return value = ~p",
               [Mod, Opts, Error]),
            error
    catch E:R:St ->
            ets:delete(modules, {Mod, Scope}),
            ?LOG_ERROR(
               "Failed to load module ~p:~n"
               "** Options = ~p~n"
               "** ~s",
               [Mod, Opts, format_exception(2, E, R, St)]),
            error
    end.

-spec reload(module(), scope(), options(), options(), pos_integer()) -> ok | error.
reload(Mod, Scope, NewOpts, OldOpts, Order) ->
    case is_exported(Mod, reload, 2) of
        false -> ok;
        true ->
            try Mod:reload(NewOpts, OldOpts) of
                ok ->
                    do_load(Mod, Scope, NewOpts, Order);
                {ok, ModifiedOpts} when is_map(ModifiedOpts) ->
                    do_load(Mod, Scope, ModifiedOpts, Order);
                Error ->
                    ?LOG_ERROR(
                       "Unexpected return value from ~s:reload/2:~n"
                       "** New options = ~p~n"
                       "** Old options = ~p~n"
                       "** Return value = ~p",
                       [Mod, NewOpts, OldOpts, Error]),
                    error
            catch E:R:St ->
                    ?LOG_ERROR(
                       "Failed to reload module ~p:~n"
                       "** New options = ~p~n"
                       "** Old options = ~p~n"
                       "** ~s",
                       [Mod, NewOpts, OldOpts, format_exception(2, E, R, St)]),
                    error
            end
    end.

-spec do_load(module(), scope(), options(), pos_integer()) -> ok.
do_load(Mod, Scope, Opts, Order) ->
    true = ets:insert(modules, {{Mod, Scope}, Opts, Order}),
    ok.

-spec unload(module(), scope(), options()) -> ok.
unload(Mod, Scope, Opts) ->
    case is_exported(Mod, unload, 1) of
        false -> ok;
        true ->
            try Mod:unload(Opts) of
                _ -> ok
            catch E:R:St ->
                    ?LOG_ERROR(
                      "Failed to unload module ~p:~n"
                      "** Options = ~p~n"
                      "** ~s",
                      [Mod, Opts, format_exception(2, E, R, St)])
            end
    end,
    ets:delete(modules, {Mod, Scope}),
    ok.

-spec merge_opts({module(), options()}) -> {module(), options()}.
merge_opts({Mod, Opts}) ->
    Defaults = case is_exported(Mod, defaults, 0) of
                   true -> Mod:defaults();
                   false -> undefined
               end,
    Required = case is_exported(Mod, required, 0) of
                   true -> Mod:required();
                   false -> []
               end,
    lists:foreach(
      fun(Opt) ->
              case maps:is_key(Opt, Opts) of
                  true -> ok;
                  false ->
                      erlang:throw({?MODULE, {missing_required_module_option, Mod, Opt}})
              end
      end, Required),
    case Defaults of
        undefined ->
            {Mod, Opts};
        _ ->
            Known = maps:keys(Defaults) ++ Required,
            lists:foreach(
              fun(Opt) ->
                      case lists:member(Opt, Known) of
                          true -> ok;
                          false ->
                              erlang:throw({?MODULE, {unknown_module_option, Mod, Opt}})
                      end
              end, maps:keys(Opts)),
            {Mod, maps:merge(Defaults, Opts)}
    end.

-spec sort_modules([{module(), options()}]) -> [{module(), options(), pos_integer()}].
sort_modules(ModOpts) ->
    G = digraph:new([acyclic]),
    lists:foreach(
      fun({Mod, Opts}) ->
              digraph:add_vertex(G, Mod, Opts),
              Deps = case is_exported(Mod, depends, 1) of
                         true -> Mod:depends(Opts);
                         false -> []
                     end,
              lists:foreach(
                fun(DepMod) ->
                        case lists:keyfind(DepMod, 1, ModOpts) of
                            false ->
                                erlang:throw({?MODULE, {missing_module_dep, Mod, DepMod}});
                            {DepMod, DepOpts} ->
                                digraph:add_vertex(G, DepMod, DepOpts),
                                case digraph:add_edge(G, DepMod, Mod) of
                                    {error, {bad_edge, Path}} ->
                                        warn_cyclic_dep(Path);
                                    _ ->
                                        ok
                                end
                        end
                end, Deps)
      end, ModOpts),
    {Result, _} = lists:mapfoldl(
                    fun(V, Order) ->
                            {M, O} = digraph:vertex(G, V),
                            {{M, O, Order}, Order+1}
                    end, 1, digraph_utils:topsort(G)),
    digraph:delete(G),
    Result.

-spec is_exported(module(), atom(), arity()) -> boolean().
is_exported(Mod, Fun, Arity) ->
    _ = code:ensure_loaded(Mod),
    erlang:function_exported(Mod, Fun, Arity).

%%%===================================================================
%%% Start/stop handlers
%%%===================================================================
-spec start(gen_server | gen_statem, module(), atom()) ->
                   {ok, pid()} | {error, {already_started, pid()} | term()}.
start(Behaviour, Mod, Proc) ->
    Spec = #{id => Proc,
             start => {Behaviour, start_link,
                       [{local, Proc}, Mod, [], []]},
             restart => transient,
             shutdown => timer:minutes(1),
             type => worker,
             modules => [Mod]},
    supervisor:start_child(gen_mod_sup, Spec).

-spec stop(atom()) -> ok | {error, term()}.
stop(Proc) ->
    _ = supervisor:terminate_child(gen_mod_sup, Proc),
    supervisor:delete_child(gen_mod_sup, Proc).

%%%===================================================================
%%% Logging/formatting
%%%===================================================================
-spec warn_cyclic_dep([atom(), ...]) -> ok.
warn_cyclic_dep(Path) ->
    ?LOG_WARNING(
      "Cyclic dependency detected between modules ~s. "
      "This is either a bug, or the modules are not "
      "supposed to work together in this configuration. "
      "The modules will still be loaded though, however "
      "their startup order is unpredictable.",
      [format_cycle(Path)]).

-spec format_cycle([atom(), ...]) -> iolist().
format_cycle([M1]) ->
    atom_to_list(M1);
format_cycle([M1, M2]) ->
    [atom_to_list(M1), " and ", atom_to_list(M2)];
format_cycle([M|Ms]) ->
    atom_to_list(M) ++ ", " ++ format_cycle(Ms).

format_exception(Level, Class, Reason, Stacktrace) ->
    erl_error:format_exception(
      Level, Class, Reason, Stacktrace,
      fun(_M, _F, _A) -> false end,
      fun(Term, I) ->
              io_lib:print(Term, I, 80, -1)
      end).

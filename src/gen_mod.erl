%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @doc
%%%
%%% @end
%%% Created : 25 Jan 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(gen_mod).

%% API
-export([load/1, unload/0]).
-export([start_server/1, start_server/2, stop_server/1]).
-export([start_statem/1, start_statem/2, stop_statem/1]).
-export([is_loaded/1, get_opt/2, loaded/0]).
-export_type([options/0]).

-include_lib("kernel/include/logger.hrl").

-type options() :: map().

-callback load(options()) -> ok | {ok, pid()}.
-callback unload(options()) -> any().
-callback reload(options(), options()) -> ok.
-callback defaults() -> options().
-callback required() -> [atom()].
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

-spec load([{module(), options()}]) -> ok.
load(ModOpts) ->
    ModOpts1 = lists:map(fun merge_opts/1, ModOpts),
    ModOpts2 = sort_modules(ModOpts1),
    lists:foreach(
      fun({Mod, Opts, Order}) ->
              case ets:lookup(modules, Mod) of
                  [{Mod, OldOpts, OldOrder}] ->
                      case OldOpts of
                          Opts when Order == OldOrder ->
                              ok;
                          Opts ->
                              ets:insert(modules, {Mod, Opts, Order});
                          _ ->
                              reload(Mod, Opts, OldOpts, Order)
                      end;
                  [] ->
                      load(Mod, Opts, Order)
              end
      end, ModOpts2),
    lists:foreach(
      fun({Mod, Opts}) ->
              case lists:keymember(Mod, 1, ModOpts2) of
                  true -> ok;
                  false -> unload(Mod, Opts)
              end
      end, lists:reverse(loaded())).

-spec unload() -> ok.
unload() ->
    lists:foreach(
      fun({Mod, Opts}) ->
              unload(Mod, Opts)
      end, lists:reverse(loaded())).

-spec is_loaded(module()) -> boolean().
is_loaded(Mod) ->
    ets:member(modules, Mod).

-spec get_opt(atom(), options() | module()) -> term().
get_opt(Opt, Opts) when is_map(Opts) ->
    maps:get(Opt, Opts);
get_opt(Opt, Mod) ->
    Opts = ets:lookup_element(modules, Mod, 2),
    maps:get(Opt, Opts).

-spec loaded() -> [{module(), options()}].
loaded() ->
    [{Mod, Opts} || {Mod, Opts, _Order} <- lists:keysort(3, ets:tab2list(modules))].

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec load(module(), options(), pos_integer()) -> ok.
load(Mod, Opts, Order) ->
    ets:insert(modules, {Mod, Opts, Order}),
    try Mod:load(Opts) of
        ok -> ok;
        {ok, Pid} when is_pid(Pid) -> ok;
        Error ->
            ets:delete(modules, Mod),
            ?LOG_ERROR(
              "Unexpected return value from ~s:load/1:~n"
              "** Options = ~p~n"
              "** Return value = ~p",
              [Mod, Opts, Error])
    catch E:R:St ->
            ets:delete(modules, Mod),
            ?LOG_ERROR(
              "Failed to load module ~p:~n"
              "** Options = ~p~n"
              "** ~s",
              [Mod, Opts, format_exception(2, E, R, St)])
    end.

-spec reload(module(), options(), options(), pos_integer()) -> ok.
reload(Mod, NewOpts, OldOpts, Order) ->
    case is_exported(Mod, reload, 2) of
        false -> ok;
        true ->
            try Mod:reload(NewOpts, OldOpts) of
                ok -> ok;
                Error ->
                    ?LOG_ERROR(
                      "Unexpected return value from ~s:reload/2:~n"
                      "** New options = ~p~n"
                      "** Old options = ~p~n"
                      "** Return value = ~p",
                      [Mod, NewOpts, OldOpts, Error])
            catch E:R:St ->
                    ?LOG_ERROR(
                      "Failed to reload module ~p:~n"
                      "** New options = ~p~n"
                      "** Old options = ~p~n"
                      "** ~s",
                      [Mod, NewOpts, OldOpts, format_exception(2, E, R, St)])
            end
    end,
    ets:insert(modules, {Mod, NewOpts, Order}),
    ok.

-spec unload(module(), options()) -> ok.
unload(Mod, Opts) ->
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
    ets:delete(modules, Mod),
    ok.

-spec merge_opts({module(), options()}) -> {module(), options()}.
merge_opts({Mod, Opts}) ->
    Defaults = case is_exported(Mod, defaults, 0) of
                   true -> Mod:defaults();
                   false -> #{}
               end,
    Required = case is_exported(Mod, required, 0) of
                   true -> Mod:required();
                   false -> []
               end,
    Known = maps:keys(Defaults) ++ Required,
    lists:foreach(
      fun(Opt) ->
              case lists:member(Opt, Known) of
                  true -> ok;
                  false ->
                      erlang:error({unknown_module_option, Mod, Opt})
              end
      end, maps:keys(Opts)),
    lists:foreach(
      fun(Opt) ->
              case maps:is_key(Opt, Opts) of
                  true -> ok;
                  false ->
                      erlang:error({missing_required_module_option, Mod, Opt})
              end
      end, Required),
    {Mod, maps:merge(Defaults, Opts)}.

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
                                erlang:error({missing_module_dep, Mod, DepMod});
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

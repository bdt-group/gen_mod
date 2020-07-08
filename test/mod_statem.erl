%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(mod_statem).

-behaviour(gen_statem).
-behaviour(gen_mod).

%% API
-export([start_link/0, load/1, unload/1, defaults/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).
-export([state_name/3]).

-record(data, {}).

%%%===================================================================
%%% API
%%%===================================================================
load(_) ->
    gen_mod:start_statem(?MODULE).

unload(_) ->
    gen_mod:stop_statem(?MODULE).

defaults() ->
    #{a => 1}.

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================
callback_mode() -> state_functions.

init([]) ->
    process_flag(trap_exit, true),
    _ = gen_mod:get_opt(a, ?MODULE),
    {ok, state_name, #data{}}.

state_name({call,Caller}, _Msg, Data) ->
    {next_state, state_name, Data, [{reply,Caller,ok}]}.

terminate(_Reason, _State, _Data) ->
    void.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

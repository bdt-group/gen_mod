%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(mod_test_dep).

-behaviour(gen_mod).

%% API
-export([load/1, depends/1, defaults/0, required/0]).

%%%===================================================================
%%% API
%%%===================================================================
load(_) ->
    ok.

depends(_) ->
    [mod_test].

defaults() ->
    #{}.

required() ->
    [].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%
%%% @end
%%% Created : 28 Apr 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(mod_test).

-behaviour(gen_mod).

%% API
-export([load/1]).
-export([reload/2]).
-export([unload/1]).

%%%===================================================================
%%% API
%%%===================================================================
load(#{load := ok_opts} = Opts) ->
    {ok, Opts};
load(#{load := ok_modify_opts} = Opts) ->
    {ok, Opts#{load_modified => true}};
load(#{load := error}) ->
    error;
load(#{load := exception}) ->
    erlang:error({?MODULE, load_exception});
load(_) ->
    ok.

reload(#{reload := ok_opts} = NewOpts, _) ->
    {ok, NewOpts};
reload(#{reload := ok_modify_opts} = NewOpts, _) ->
    {ok, NewOpts#{reload_modified => true}};
reload(#{reload := error}, _) ->
    error;
reload(#{reload := exception}, _) ->
    erlang:error({?MODULE, reload_exception});
reload(_, _) ->
    ok.

unload(_) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

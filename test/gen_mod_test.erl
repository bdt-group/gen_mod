%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%
%%% @end
%%% Created : 29 Apr 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(gen_mod_test).

-include_lib("eunit/include/eunit.hrl").

start_test() ->
    ?assertEqual(ok, gen_mod:start()).

double_start_test() ->
    ?assertEqual(ok, gen_mod:start()).

load_unload_test() ->
    ?assertEqual(ok, gen_mod:load([{mod_test, #{}}])),
    ?assertEqual(true, gen_mod:is_loaded(mod_test)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual(false, gen_mod:is_loaded(mod_test)).

loaded_dep_test() ->
    ?assertEqual(ok, gen_mod:load([{mod_test_dep, #{}}, {mod_test, #{}}])),
    ?assertEqual([{undefined, [{mod_test, #{}}, {mod_test_dep, #{}}]}], gen_mod:loaded()),
    ?assertEqual(ok, gen_mod:unload()).

reload_test() ->
    ?assertEqual(ok, gen_mod:load([{mod_test, #{}}], a)),
    ?assertEqual(ok, gen_mod:load([{mod_test, #{}}], b)),
    ?assertEqual(ok, gen_mod:load([{mod_test, #{a => 1}}], a)),
    ?assertEqual(ok, gen_mod:load([{mod_test, #{b => 2}}], b)),
    ?assertEqual([{mod_test, #{a => 1}}], gen_mod:loaded(a)),
    ?assertEqual([{mod_test, #{b => 2}}], gen_mod:loaded(b)),
    ?assertEqual(ok, gen_mod:unload(a)),
    ?assertEqual(ok, gen_mod:unload(b)).

unload_test() ->
    ?assertEqual(ok, gen_mod:load([{mod_test, #{}}, {mod_test_dep, #{}}])),
    ?assertEqual(ok, gen_mod:load([{mod_test, #{}}])),
    ?assertEqual([{mod_test, #{}}], gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:load([])),
    ?assertEqual([], gen_mod:loaded()).

cycle_test() ->
    ?assertEqual(ok, gen_mod:load([{mod_cycle1, #{}}, {mod_cycle2, #{}}])),
    ?assertEqual(true, gen_mod:is_loaded(mod_cycle1)),
    ?assertEqual(true, gen_mod:is_loaded(mod_cycle2)),
    ?assertEqual(ok, gen_mod:unload()).

get_opt_test() ->
    Opts = #{a => 1},
    ?assertEqual(ok, gen_mod:load([{mod_test, Opts}])),
    ?assertEqual(1, gen_mod:get_opt(a, mod_test)),
    ?assertEqual(1, gen_mod:get_opt(a, Opts)),
    ?assertError(_, gen_mod:get_opt(b, mod_test)),
    ?assertError(_, gen_mod:get_opt(b, Opts)),
    ?assertEqual(ok, gen_mod:unload()).

server_test() ->
    ?assertEqual(ok, gen_mod:load([{mod_server, #{}}])),
    ?assertEqual(true, gen_mod:is_loaded(mod_server)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual(false, gen_mod:is_loaded(mod_server)).

statem_test() ->
    ?assertEqual(ok, gen_mod:load([{mod_statem, #{}}])),
    ?assertEqual(true, gen_mod:is_loaded(mod_statem)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual(false, gen_mod:is_loaded(mod_statem)).

stop_test() ->
    ?assertEqual(ok, gen_mod:stop()).

double_stop_test() ->
    ?assertEqual(ok, gen_mod:stop()).

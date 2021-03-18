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
    ?assertEqual([{mod_test, #{}}], gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual(false, gen_mod:is_loaded(mod_test)),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(ok, gen_mod:load([{mod_test, #{load => ok_opts}}])),
    ?assertEqual(true, gen_mod:is_loaded(mod_test)),
    ?assertEqual([{mod_test, #{load => ok_opts}}], gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual(false, gen_mod:is_loaded(mod_test)),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(ok, gen_mod:load([{mod_test, #{load => ok_modify_opts}}])),
    ?assertEqual(true, gen_mod:is_loaded(mod_test)),
    ?assertEqual([{mod_test, #{load => ok_modify_opts, load_modified => true}}],
                 gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual(false, gen_mod:is_loaded(mod_test)),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual({error, {failed_modules, [mod_test]}},
                 gen_mod:load([{mod_test, #{load => error}}, {mod_test1, #{b => 2}}])),
    ?assertEqual([false, true], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test1]]),
    ?assertEqual([{mod_test1, #{a => 1, b => 2}}], gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual([false, false], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test1]]),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual({error, {failed_modules, [mod_test]}},
                 gen_mod:load([{mod_test, #{load => exception}}, {mod_test1, #{b => 2}}])),
    ?assertEqual([false, true], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test1]]),
    ?assertEqual([{mod_test1, #{a => 1, b => 2}}], gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual([false, false], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test1]]),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(ok, gen_mod:load([{mod_test, #{}}, {mod_test1, #{a => 3, b => 2}}])),
    ?assertEqual([true, true], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test1]]),
    ?assertEqual([{mod_test1, #{a => 3, b => 2}}, {mod_test, #{}}], gen_mod:loaded(undefined)),
     ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual([false, false], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test1]]),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(ok, gen_mod:load([{mod_test1, #{b => 2}}])),
    ?assertEqual(true, gen_mod:is_loaded(mod_test1)),
    ?assertEqual([{mod_test1, #{a => 1, b => 2}}], gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual(false, gen_mod:is_loaded(mod_test1)),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(
       {error, {missing_required_module_option, mod_test1, b}},
       gen_mod:load([{mod_test1, #{}}, {mod_test, #{}}])),
    ?assertEqual([false, false], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test1]]),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(
       {error, {unknown_module_option, mod_test1, c}},
       gen_mod:load([{mod_test1, #{b => 2, c => 3}}])),
    ?assertEqual(false, gen_mod:is_loaded(mod_test1)),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(ok, gen_mod:load([{mod_server, #{a => 1}}])),
    ?assertEqual(true, gen_mod:is_loaded(mod_server)),
    ?assertEqual([{mod_server, #{a => 1}}], gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual(false, gen_mod:is_loaded(mod_server)),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(ok, gen_mod:load([{mod_server, #{a => 1, b => 2}}])),
    ?assertEqual(true, gen_mod:is_loaded(mod_server)),
    ?assertEqual([{mod_server, #{a => 1, b => 2}}], gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual(false, gen_mod:is_loaded(mod_server)),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(
       {error, {missing_required_module_option, mod_server, a}},
       gen_mod:load([{mod_server, #{b => 2}}])),
    ?assertEqual(false, gen_mod:is_loaded(mod_server)),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(ok, gen_mod:load([{mod_statem, #{a => 2}}])),
    ?assertEqual(true, gen_mod:is_loaded(mod_statem)),
    ?assertEqual([{mod_statem, #{a => 2}}], gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual(false, gen_mod:is_loaded(mod_statem)),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(ok, gen_mod:load([{mod_statem, #{}}])),
    ?assertEqual(true, gen_mod:is_loaded(mod_statem)),
    ?assertEqual([{mod_statem, #{a => 1}}], gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual(false, gen_mod:is_loaded(mod_statem)),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(
       {error, {unknown_module_option, mod_statem, b}},
       gen_mod:load([{mod_statem, #{b => 2}}])),
    ?assertEqual(false, gen_mod:is_loaded(mod_statem)),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(ok, gen_mod:load([{mod_test, #{}}, {mod_test_dep, #{}}])),
    ?assertEqual([true, true], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test_dep]]),
    ?assertEqual([{mod_test, #{}}, {mod_test_dep, #{}}], gen_mod:loaded(undefined)),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual([false, false], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test_dep]]),
    ?assertEqual([], gen_mod:loaded(undefined)),

    ?assertEqual(
       {error, {unknown_module_option, mod_test_dep, a}},
       gen_mod:load([{mod_test, #{}}, {mod_test_dep, #{a => 1}}])),
    ?assertEqual([false, false], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test_dep]]),
    ?assertEqual([], gen_mod:loaded(undefined)).

loaded_dep_test() ->
    ?assertEqual(ok, gen_mod:load([{mod_test_dep, #{}}, {mod_test, #{}}])),
    ?assertEqual([true, true], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test_dep]]),
    ?assertEqual(ok, gen_mod:unload()),
    ?assertEqual([false, false], [gen_mod:is_loaded(M) || M <- [mod_test, mod_test_dep]]),

    ?assertEqual(
       {error, {missing_module_dep, mod_test_dep, mod_test}},
       gen_mod:load([{mod_test_dep, #{}}])),
    ?assertEqual(false, gen_mod:is_loaded(mod_test_dep)).

reload_test() ->
    ?assertEqual(ok, gen_mod:load([{mod_test, #{}}, {mod_test1, #{b => 2}}], s1)),
    ?assertEqual(ok, gen_mod:load([{mod_test, #{}}], s2)),
    ?assertEqual([
                  {mod_test1, #{a => 1, b => 2}},
                  {mod_test, #{}}],
                 gen_mod:loaded(s1)),
    ?assertEqual([{mod_test, #{}}], gen_mod:loaded(s2)),

    ?assertEqual(ok, gen_mod:load([{mod_test, #{reload => ok_opts}}], s1)),
    ?assertEqual(ok, gen_mod:load([{mod_test, #{b => 2}}], s2)),
    ?assertEqual([{mod_test, #{reload => ok_opts}}], gen_mod:loaded(s1)),
    ?assertEqual([{mod_test, #{b => 2}}], gen_mod:loaded(s2)),

    ?assertEqual(ok, gen_mod:load([{mod_test, #{reload => ok_modify_opts}}], s1)),
    ?assertEqual([{mod_test, #{reload => ok_modify_opts, reload_modified => true}}],
                 gen_mod:loaded(s1)),

    ?assertEqual({error, {failed_modules, [mod_test]}},
                  gen_mod:load([{mod_test, #{reload => error}}], s1)),
    ?assertEqual([
                  {mod_test, #{reload => ok_modify_opts, reload_modified => true}}],
                 gen_mod:loaded(s1)),

    ?assertEqual({error, {failed_modules, [mod_test]}},
                  gen_mod:load([{mod_test, #{reload => exception}}], s1)),
    ?assertEqual([
                  {mod_test, #{reload => ok_modify_opts, reload_modified => true}}],
                 gen_mod:loaded(s1)),

    ?assertEqual(ok, gen_mod:unload(s1)),
    ?assertEqual(ok, gen_mod:unload(s2)),
    ?assertEqual([], gen_mod:loaded()).

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
    ?assertEqual(ok, gen_mod:load([{mod_server, #{a => 1}}])),
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

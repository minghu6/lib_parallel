-module(test_lib_parallel).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

pmap_test() ->
    F = fun(X) -> X + 1 end,
    ?assertEqual(lib_parallel:pmap(F, lists:seq(1, 200)), lists:map(F, lists:seq(1, 200))).

dpmap_test() ->
    F = fun(X) -> X + 1 end,
    ?assertEqual(sets:from_list(lib_parallel:pmap(F, lists:seq(1, 200))), sets:from_list(lists:map(F, lists:seq(1, 200)))).
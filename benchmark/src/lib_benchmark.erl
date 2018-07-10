-module(lib_benchmark).

-compile(export_all).

%%====================================================================
%% API functions
%%====================================================================
loop_count(F, N) ->
    loop_count0(F, N, {0, 0}).

loop_stat(F, N) ->
    loop_count(fun() ->
        F(),
        {_, T} = statistics(runtime),
        T
    end, N).

loop_tc(F, N) ->
    loop_count(fun() ->
        {T, _} = timer:tc(F),
        T
    end, N).
%%====================================================================
%% Internal functions
%%====================================================================
loop_count0(F, N, {AccN, AccT}) when N /= AccN ->
    T0 = F(),
    loop_count0(F, N, {AccN + 1, AccT + T0});
loop_count0(_, _, {AccN, AccT}) ->
    AccT / AccN.



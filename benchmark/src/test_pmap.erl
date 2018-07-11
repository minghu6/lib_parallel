-module(test_pmap).

-compile(export_all).

gen_list(F, N) ->
    gen_list0(F, N, []).

gen_list0(_, 0, Acc) -> Acc;
gen_list0(F, N, Acc) ->
    gen_list0(F, N-1, [F()|Acc]).

map_and_pmap_test_tc() ->
    NT = 5,
    NL = 20,
    L = gen_list(fun() ->
        A=matrix:gen(100, 100),
        B=matrix:gen(100, 100),
        {A, B} 
    end, NL),

    T_pmap = lib_benchmark:loop_tc(fun() -> lib_parallel:pmap(fun(X) -> 
        {A, B} = X,
        matrix:multiple(A, B) end, L) end, NT),

    io:format("PMAP tc count ~.1f us test ~.10# times matrix multiple with 100*100 on list len ~.10#~n", [T_pmap, NT, NL]),
    
    T_map = lib_benchmark:loop_tc(fun() -> lists:map(fun(X) -> 
        {A, B} = X,
        matrix:multiple(A, B) end, L) end, NT),
    io:format("MAP tc count ~.1f us test ~.10# times matrix multiple with 100*100 on list len ~.10#~n", [T_map, NT, NL]).


map_and_pmap_test_stat() ->
    NT = 3,
    NL = 20,
    L = gen_list(fun() ->
        A=matrix:gen(100, 100),
        B=matrix:gen(100, 100),
        {A, B} 
    end, NL),

    T_pmap = lib_benchmark:loop_stat(fun() -> lib_parallel:pmap(fun(X) -> 
        {A, B} = X,
        matrix:multiple(A, B) end, L) end, NT),

    io:format("PMAP stat count ~.1f us test ~.10# times matrix multiple with 100*100 on list len ~.10#~n", [T_pmap, NT, NL]),

    T_map = lib_benchmark:loop_stat(fun() -> lists:map(fun(X) -> 
        {A, B} = X,
        matrix:multiple(A, B) end, L) end, NT),
    io:format("MAP stat count ~.1f us test ~.10# times matrix multiple with 100*100 on list len ~.10#~n", [T_map, NT, NL]).
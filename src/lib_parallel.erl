-module(lib_parallel).

%% API exports
-export([mapreduce/4, pmap/2, pmap/3, pmap2/2]).

%%====================================================================
%% API functions
%%====================================================================
pmap(F, L, K) ->
    Len = length(L),
    case Len > K of
      true ->
      {KT, Remain} = lists:split(K, L),
      pmap(F, KT) ++ pmap(F, Remain, K);
      false -> pmap(F, L)
    end.

pmap(F, L) ->
    S = self(),
    Ref = erlang:make_ref(),
    Pids = lists:map(fun (I) ->
                spawn_link(fun () -> do_f(S, Ref, F, I) end)
             end,
             L),
    gather(Pids, Ref).

pmap2(F, L) ->
    S = self(),
    Ref = erlang:make_ref(),
    lists:foreach(fun (I) ->
              spawn_link(fun () -> do_f2(S, Ref, F, I) end)
          end,
          L),
    gather2(length(L), Ref, []).

mapreduce(F1, F2, Acc0, L) ->
        Pid = spawn_link(fun() -> reduce(self(), F1, F2, Acc0, L) end),
        
        receive
            {Pid, Result} ->
            Result
        end.
%%====================================================================
%% Internal functions
%%====================================================================
do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, catch F(I)}.

gather([Pid | T], Ref) ->
    receive {Pid, Ref, Ret} -> [Ret | gather(T, Ref)] end;
gather([], _) -> [].

do_f2(Parent, Ref, F, I) -> Parent ! {Ref, catch F(I)}.

gather2(0, _, L) -> L;
gather2(N, Ref, L) ->
    receive
      {Ref, Ret} -> gather2(N - 1, Ref, [Ret | L])
    end.

    reduce(Parent, F1, F2, Acc0, L) ->
            ReducedPid = self(),
          
            lists:foreach(fun() -> 
              spawn_link(fun(X) -> F1(ReducedPid, X) end) end, L),
            
            N = length(L),
            Dict0 = dict:new(),
            Dict1 = collect_replies(N, Dict0),
            Acc = dict:fold(F2, Acc0, Dict1),
          
            Parent ! {ReducedPid, Acc}.
          
          collect_replies(0, Dict) ->
            Dict;
          collect_replies(N, Dict) ->
            receive
              {Key, Value} ->
                case dict:is_key(Key, Dict) of
                  true ->
                    Dict1 = dict:append(Key, Value, Dict),
                    collect_replies(N-1, Dict1);
                  false ->
                    Dict1 = dict:store(Key, Value, Dict),
                    collect_replies(N-1, Dict1)
                end;
              {'Exit', _, _Why} ->
                io:format("~p~n", [_Why]),
                collect_replies(N-1, Dict)
              end.
          

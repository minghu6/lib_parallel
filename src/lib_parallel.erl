-module(lib_parallel).

%% API exports
-export([mapreduce/4, pmap/2, dpmap/2]).

%%====================================================================
%% API functions
%%====================================================================
pmap(F, L) ->
    K = erlang:system_info(logical_processors_available) * 10,
    pmap(F, L, K).

%% disorder parallel map
dpmap(F, L) ->
    S = self(),
    Ref = erlang:make_ref(),
    lists:foreach(fun (I) ->
              spawn_link(fun () -> ddo_f2(S, Ref, F, I) end)
          end,
          L),
    dgather2(length(L), Ref, []).

mapreduce(F1, F2, Acc0, L) ->
        Pid = spawn_link(fun() -> reduce(self(), F1, F2, Acc0, L) end),
        
        receive
            {Pid, Result} ->
            Result
        end.
%%====================================================================
%% Internal functions
%%====================================================================
pmap(F, L, K) when L /= []->
    {KT, Remain} = list_split(K, L),
    pmap0(F, KT) ++ pmap(F, Remain, K);
pmap(_, [], _) -> [].

list_split(N, L) ->
  try lists:split(N, L)
  catch _:_->
    {L, []}
  end.

pmap0(F, L) ->
    S = self(),
    Ref = erlang:make_ref(),
    Pids = lists:map(fun (I) ->
                spawn_link(fun () -> do_f(S, Ref, F, I) end)
             end,
             L),
    gather(Pids, Ref).

do_f(Parent, Ref, F, I) ->
    Parent ! {self(), Ref, catch F(I)}.

gather([Pid | T], Ref) ->
    receive {Pid, Ref, Ret} -> [Ret | gather(T, Ref)] end;
gather([], _) -> [].

ddo_f2(Parent, Ref, F, I) -> Parent ! {Ref, catch F(I)}.

dgather2(0, _, L) -> L;
dgather2(N, Ref, L) ->
    receive
      {Ref, Ret} -> dgather2(N - 1, Ref, [Ret | L])
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
          

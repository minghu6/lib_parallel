-module(test_pmap).



-compile(export_all).

shuffle(List) -> shuffle(List, []).

shuffle([], Acc) -> Acc;
shuffle(List, Acc) ->
    {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
    shuffle(Leading ++ T, [H | Acc]).
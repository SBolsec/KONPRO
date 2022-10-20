-module(first).
-export([addElement/2, cheapSort/2, sortList/2, sortedAdd/2, sortList/3, sortedAdd/3, addTuple/2]).

% 1.

addElement(List, Element) ->
    List ++ [Element].



% 2.

cheapSort(List, Element) -> lists:sort(List ++ [Element]).

sortList(List, Element) ->
    sort(List, Element, 1).

sort(List, Element, N) ->
    if
        length(List) < N ->
            List ++ [Element];
        true ->
            CurrentElement = lists:nth(N, List),

            if
                CurrentElement > Element ->
                    {First, Second} = lists:split(N-1, List),
                    First ++ [Element] ++ Second;
                true ->
                    sort(List, Element, N + 1)
            end
    end.


sortedAdd(List, Element) -> 
    sortedAddHelper(List, Element, []).

sortedAddHelper([First | Rest], Element, Result) when First > Element ->
    Result ++ [Element, First] ++ Rest;
sortedAddHelper([First | Rest], Element, Result) ->
    sortedAddHelper(Rest, Element, Result ++ [First]);
sortedAddHelper(List, Element, Result) when length(List) == 0 ->
    Result ++ [Element].

% 3.

sortList(List, Element, Sorter) ->
    sort(List, Element, 1, Sorter).

sort(List, Element, N, Sorter) ->
    if
        length(List) < N ->
            List ++ [Element];
        true ->
            CurrentElement = lists:nth(N, List),
            Cond = Sorter(CurrentElement, Element),

            if
                Cond ->
                    {First, Second} = lists:split(N-1, List),
                    First ++ [Element] ++ Second;
                true ->
                    sort(List, Element, N+1, Sorter)
            end
    end.

sortedAdd(List, Element, Sorter) -> 
    sortedAddHelper(List, Element, [], Sorter).

sortedAddHelper([First | Rest], Element, Result, Sorter)->
    Cond = Sorter(First, Element),
    if 
        Cond -> Result ++ [Element, First] ++ Rest;
        true -> sortedAddHelper(Rest, Element, Result ++ [First])
    end;
sortedAddHelper(List, Element, Result, _) when length(List) == 0 ->
    Result ++ [Element].


% 4.

addTuple(List, Tuple) ->
    addTupleHelper(List, Tuple, []).

addTupleHelper([{FirstKey, FirstValue} | Rest], { Key, Value }, Result) when FirstKey > Key ->
    Result ++ [{Key, Value}, {FirstKey, FirstValue}] ++ Rest;
addTupleHelper([First | Rest], Tuple, Result) ->
    addTupleHelper(Rest, Tuple, Result ++ [First]);
addTupleHelper(List, Tuple, Result) when length(List) == 0 ->
    Result ++ [Tuple].

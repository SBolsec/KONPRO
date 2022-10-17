-module(first).
-export([addElement/2, cheapSort/2, sortList/2, sortList/3]).

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

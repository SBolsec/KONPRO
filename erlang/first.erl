-module(first).
-export([addElement/2, cheapSort/2, sortList/2, sortedAdd/2, sortList/3, sortedAdd/3, addTuple/2, findKey/2, addTreeNode/2, findTreeNode/2]).

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

% 5.

findKey([{FirstKey, FirstValue}, _], Key) when FirstKey == Key ->
    FirstValue;
findKey(List, _) when length(List) == 0 ->
    null;
findKey([_ | Rest], Key) ->
    findKey(Rest, Key).


% 6.

% tree is map with:
%   - value: node value
%   - left: left child      literal null if no child
%   - right: right child    literal null if no child

addTreeNode(#{ value := CurrentValue, left := Left, right := Right }, Value) when Value < CurrentValue, Left == null ->
    Node = #{ value => Value, left => null, right => null },
    #{ value => CurrentValue, left => Node, right => Right};
addTreeNode(#{ value := CurrentValue, left := Left, right := Right }, Value) when Value < CurrentValue ->
    Node = addTreeNode(Left, Value),
    #{ value => CurrentValue, left => Node, right => Right};
addTreeNode(#{ value := CurrentValue, left := Left, right := Right }, Value) when Value > CurrentValue, Right == null ->
    Node = #{ value => Value, left => null, right => null },
    #{ value => CurrentValue, left => Left, right => Node};
addTreeNode(#{ value := CurrentValue, left := Left, right := Right }, Value) when Value > CurrentValue ->
    Node = addTreeNode(Right, Value),
    #{ value => CurrentValue, left => Left, right => Node};
addTreeNode(#{ value := CurrentValue, left := Left, right := Right }, Value) when Value == CurrentValue ->
    #{ value => CurrentValue, left => Left, right => Right};
addTreeNode(_, Value) ->
    #{ value => Value, left => null, right => null }.

findTreeNode(#{ value := Value, left := Left, right := Right }, Key) when Value == Key ->
    #{ value => Value, left => Left, right => Right };
findTreeNode(#{ value := Value, left := Left}, Key) when Key < Value ->
    findTreeNode(Left, Key);
findTreeNode(#{ value := Value, right := Right }, Key) when Key > Value ->
    findTreeNode(Right, Key);
findTreeNode(_, _) ->
    null.


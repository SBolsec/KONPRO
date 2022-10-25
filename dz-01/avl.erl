-module(avl).
-author(sbolsec).

-export([add/2, get/2]).

% Node shape: [CurrentValue, BalanceFactor, LeftChild, RightChild]
% If node has no child, that child is empty list []

add(Value, Tree) ->
    Node = get(Value, Tree),
    if
        Node /= undefined -> Tree;
        true -> addHelper(Value, Tree)
    end.

addHelper(Value, Tree) when length(Tree) == 0 ->
    [Value, 0, [], []];
addHelper(Value, [CV, BF, LC, RC]) when Value < CV, length(LC) == 0 ->
    [CV, BF - 1, addHelper(Value, LC), RC];
addHelper(Value, [CV, BF, LC, RC]) when Value > CV, length(RC) == 0 ->
    [CV, BF + 1, LC, addHelper(Value, RC)];
addHelper(Value, [CV, _, LC, RC]) when Value < CV ->
    Node = addHelper(Value, LC),
    NewBF = balanceFactor(RC) - balanceFactor(Node),
    if
        NewBF /= -2 ->
            [CV, NewBF, Node, RC];
        true ->
            [NCV, NBF, NLC, NRC] = Node,
            if
                NBF == -1 ->
                    [NCV, 0, NLC, [CV, 0, [], RC]];
                true ->
                    [NRCV, _, NRLC, _] = NRC,
                    [NRCV, 0, [NCV, 0, NRLC, []], [CV, 0, [], RC]]
            end
    end;
addHelper(Value, [CV, _, LC, RC]) when Value > CV ->
    Node = addHelper(Value, RC),
    NewBF = balanceFactor(Node) - balanceFactor(LC),
    if
        NewBF /= 2 ->
            [CV, NewBF, LC, Node];
        true ->
            [NCV, NBF, NLC, NRC] = Node,
            if
                NBF == 1 ->
                    [NCV, 0, [CV, 0, LC, []], NRC];
                true ->
                    [NLCV, _, _, NLRC] = NLC,
                    [NLCV, 0, [CV, 0, LC, []], [NCV, 0, [], NLRC]]
            end
    end.

balanceFactor(Node) when length(Node) == 0 ->
    0;
balanceFactor([_, BF | _]) when BF == 0 ->
    1;
balanceFactor([_, BF | _]) when BF < 0 ->
    BF - 1;
balanceFactor([_, BF | _]) when BF > 0 ->
    BF + 1.

get(_, Tree) when length(Tree) == 0 ->
    undefined;
get(Value, [CV | Rest]) when Value == CV ->
    [CV] ++ Rest;
get(Value, [CV, _, LC, _]) when Value < CV ->
    get(Value, LC);
get(Value, [CV, _, _, RC]) when Value > CV ->
    get(Value, RC).

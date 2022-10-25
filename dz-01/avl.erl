-module(avl).
-author(sbolsec).

-export([add/2, get/2]).

% Node shape: [CurrentValue, Height, LeftChild, RightChild]
% If node has no child, that child is empty list []

add(Value, Tree) ->
    Node = get(Value, Tree),
    if
        Node /= undefined -> Tree;
        true -> addHelper(Value, Tree)
    end.

addHelper(Value, Tree) when length(Tree) == 0 ->
    [Value, 1, [], []];
addHelper(Value, [CV, H, LC, RC]) when Value < CV, length(LC) == 0 ->
    [CV, H + 1, addHelper(Value, LC), RC];
addHelper(Value, [CV, H, LC, RC]) when Value > CV, length(RC) == 0 ->
    [CV, H + 1, LC, addHelper(Value, RC)];
addHelper(Value, [CV, H, LC, RC]) when Value < CV ->
    Node = addHelper(Value, LC),
    HL = height(Node),
    HR = height(RC),
    BalanceFactor = HR - HL,
    if
        BalanceFactor /= -2 ->
            [CV, erlang:max(HR, HL) + 1, Node, RC];
        true ->
            [NCV, NH, NLC, NRC] = Node,
            HNLC = height(NLC),
            HNRC = height(NRC),
            if
                HNLC > HNRC ->
                    [NCV, NH - 1, NLC, [CV, 0, [], RC]];
                true ->
                    [NRCV, _, NRLC, _] = NRC,
                    [NRCV, 0, [NCV, 0, NRLC, []], [CV, 0, [], RC]]
            end
    end;
addHelper(Value, [CV, H, LC, RC]) when Value > CV ->
    Node = addHelper(Value, RC),
    HL = height(LC),
    HR = height(Node),
    BalanceFactor = HR - HL,
    if
        BalanceFactor /= 2 ->
            [CV, erlang:max(HR, HL) + 1, LC, Node];
        true ->
            [NCV, NH, NLC, NRC] = Node,
            HNLC = height(NLC),
            HNRC = height(NRC),
            if
                HNLC > HNRC ->
                    [NCV, 0, [CV, 0, LC, []], NRC];
                true ->
                    [NLCV, _, _, NLRC] = NLC,
                    [NLCV, 0, [CV, 0, LC, []], [NCV, 0, [], NLRC]]
            end
    end.

height(Node) when length(Node) == 0 ->
    0;
height([_, H | _]) ->
    H.

get(_, Tree) when length(Tree) == 0 ->
    undefined;
get(Value, [CV | Rest]) when Value == CV ->
    [CV] ++ Rest;
get(Value, [CV, _, LC, _]) when Value < CV ->
    get(Value, LC);
get(Value, [CV, _, _, RC]) when Value > CV ->
    get(Value, RC).

-module(lab).
-author("sbolsec").

-export([startServer/0, serverLoop/0, utilLoop/0]).
-export([add/1, delete/1, get/1, print/0]).

% Node shape: [Value, Height, LeftChild, RightChild]
% If node has no child, that child is empty list []

% server

startServer() ->
    Pid = spawn(lab, serverLoop, []),
    Util = spawn(lab, utilLoop, []),
    register(avlServer, Pid),
    register(utilServer, Util).

serverLoop() ->
    serverLoop([]).

serverLoop(Tree) ->
    receive
        {_, {add, Value}} ->
            serverLoop(add(Value, Tree));
        {_, {delete, Value}} ->
            serverLoop(delete(Value, Tree));
        {From, {get, Value}} ->
            Node = get(Value, Tree),
            From ! Node,
            serverLoop(Tree);
        {_, {print}} ->
            serverLoop(print(Tree));
        stop ->
            utilServer ! stop,
            true
    end.

% client

startClient({get, X}) ->
    avlServer ! {self(), {get, X}},
    receive
        Result -> Result
    end;
startClient(X) ->
    avlServer ! {self(), X}.

add(Value) ->
    startClient({add, Value}).

delete(Value) ->
    startClient({delete, Value}).

get(Value) ->
    startClient({get, Value}).

print() ->
    startClient({print}).

% add

add(Value, Tree) when length(Tree) == 0 ->
    [Value, 1, [], []];
add(Value, [V | Rest]) when Value == V ->
    [V] ++ Rest;
add(Value, [V, _, LC, RC]) when Value < V ->
    balanceLeft(Value, [V, add(Value, LC), RC]);
add(Value, [V, _, LC, RC]) when Value > V ->
    balanceRight(Value, [V, LC, add(Value, RC)]).

%delete

delete(_, Node) when length(Node) == 0 ->
    [];
delete(Value, [V, _, LC, RC]) when Value == V, length(LC) == 0, length(RC) == 0 ->
    [];
delete(Value, [V, _, LC, RC]) when Value == V, length(LC) == 0 ->
    RC;
delete(Value, [V, _, LC, RC]) when Value == V, length(RC) == 0 ->
    LC;
delete(Value, [V, _, LC, RC]) when Value == V, length(LC) /= 0, length(RC) /= 0 ->
    MinNode = getMinValueNode(RC),
    [MV | _] = MinNode,
    Right = delete(MV, RC),
    [MV, nodeHeight(LC, Right), LC, Right];
delete(Value, [V, H, LC, RC]) when Value < V ->
    LeftChild = delete(Value, LC),
    Node = [V, H, LeftChild, RC],
    BalanceFactor = balanceFactor(Node),
    if
        length(LeftChild) == 0, BalanceFactor > 1 ->
            leftRotate(Node);
        length(LeftChild) == 0 ->
            Node;
        true ->
            balanceLeft(Value, [V, LeftChild, RC])
    end;
delete(Value, [V, H, LC, RC]) when Value > V ->
    RightChild = delete(Value, RC),
    Node = [V, H, LC, RightChild],
    BalanceFactor = balanceFactor(Node),
    if
        length(RightChild) == 0, BalanceFactor < -1 ->
            rightRotate(Node);
        length(RightChild) == 0 ->
            Node;
        true ->
            balanceRight(Value, [V, LC, RightChild])
    end.

% get

get(_, Tree) when length(Tree) == 0 ->
    undefined;
get(Value, [CV | Rest]) when Value == CV ->
    [CV] ++ Rest;
get(Value, [CV, _, LC, _]) when Value < CV ->
    get(Value, LC);
get(Value, [CV, _, _, RC]) when Value > CV ->
    get(Value, RC).

% print
print(Tree) ->
    io:format("~w~n", [Tree]),
    Tree.

% utility

balanceLeft(Value, [V, LC, RC]) ->
    Height = nodeHeight(LC, RC),
    Node = [V, Height, LC, RC],
    BalanceFactor = balanceFactor(Node),
    [LeftValue | _] = LC,
    if
        BalanceFactor < -1, Value < LeftValue ->
            rightRotate(Node);
        BalanceFactor < -1, Value > LeftValue ->
            L = leftRotate(LC),
            rightRotate([V, Height, L, RC]);
        true ->
            Node
    end.

balanceRight(Value, [V, LC, RC]) ->
    Height = nodeHeight(LC, RC),
    Node = [V, Height, LC, RC],
    BalanceFactor = balanceFactor(Node),
    [RightValue | _] = RC,
    if
        BalanceFactor > 1, Value > RightValue ->
            leftRotate(Node);
        BalanceFactor > 1, Value < RightValue ->
            R = rightRotate(RC),
            leftRotate([V, Height, LC, R]);
        true ->
            Node
    end.

leftRotate(Node) ->
    utilServer ! {self(), {leftRotate, Node}},
    receive
        Result -> Result
    end.

leftRotateUtil([V, _, LC, RC]) ->
    [RV, _, RLC, RRC] = RC,
    Left = [V, nodeHeight(LC, RLC), LC, RLC],
    [RV, nodeHeight(Left, RRC), Left, RRC].

rightRotate([V, _, LC, RC]) ->
    [LV, _, LLC, LRC] = LC,
    Right = [V, nodeHeight(LRC, RC), LRC, RC],
    [LV, nodeHeight(LLC, Right), LLC, Right].

height(Node) when length(Node) == 0 ->
    0;
height([_, H | _]) ->
    H.

balanceFactor([_, _, LeftChild, RightChild]) ->
    height(RightChild) - height(LeftChild).

nodeHeight(LeftChild, RightChild) ->
    1 + erlang:max(height(LeftChild), height(RightChild)).

getMinValueNode([V, H, LC, RC]) when length(LC) == 0 ->
    [V, H, LC, RC];
getMinValueNode([_, _, LC, _]) ->
    getMinValueNode(LC).

% lab

utilLoop() ->
    io:format("Util process~n", []),
    receive
        {From, {leftRotate, Value}} ->
            Node = leftRotateUtil(Value),
            From ! Node,
            utilLoop();
        stop ->
            true
    end.

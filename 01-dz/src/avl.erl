-module(avl).
-author("sbolsec").

-export([start/0]).
-export([add/2, delete/2, get/2]).

% Node shape: [Value, Height, LeftChild, RightChild]
% If node has no child, that child is empty list []


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
  if 
    length(LeftChild) == 0 ->
      [V, H, LeftChild, RC];
    true ->
      balanceLeft(Value, [V, LeftChild, RC])
  end;
delete(Value, [V, H, LC, RC]) when Value > V ->
  RightChild = delete(Value, RC),
  if
    length(RightChild) == 0 ->
      [V, H, LC, RightChild];
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

leftRotate([V, _, LC, RC]) ->
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



% test

start() ->
  R1 = add(20, []),
  R2 = add(35, R1),
  R3 = add(40, R2),
  R4 = add(10, R3),
  R5 = add(17, R4),
  R6 = add(18, R5),
  R7 = add(19, R6),
  R8 = add(27, R7),
  R9 = add(24, R8),
  R10 = add(21, R9),
  R11 = add(21, R10),
  io:format("~w~n", [R11]).

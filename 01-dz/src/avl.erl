%%%-------------------------------------------------------------------
%%% @author sbolsec
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Oct 2022 19:27
%%%-------------------------------------------------------------------
-module(avl).
-author("sbolsec").

%% API
-export([start/0]).
-export([add/2, get/2]).

% Node shape: [Value, Height, LeftChild, RightChild]
% If node has no child, that child is empty list []

add(Value, Tree) ->
  Node = get(Value, Tree),
  if
    Node /= undefined -> Tree;
    true -> addHelper(Value, Tree)
  end.

addHelper(Value, Tree) when length(Tree) == 0 ->
  [Value, 1, [], []];
addHelper(Value, [V, _, LC, RC]) when Value < V ->
  LeftChild = addHelper(Value, LC),
  Height = nodeHeight(LeftChild, RC),
  Node = [V, Height, LeftChild, RC],
  BalanceFactor = balanceFactor(Node),
  [LeftValue | _] = LeftChild,
  if
    BalanceFactor < -1, Value < LeftValue ->
      rightRotate(Node);
    BalanceFactor < -1, Value > LeftValue ->
      L = leftRotate(LeftChild),
      rightRotate([V, Height, L, RC]);
    true ->
      Node
  end;
addHelper(Value, [V, _, LC, RC]) when Value > V ->
  RightChild = addHelper(Value, RC),
  Height = nodeHeight(LC, RightChild),
  Node = [V, Height, LC, RightChild],
  BalanceFactor = balanceFactor(Node),
  [RightValue | _] = RightChild,
  if
    BalanceFactor > 1, Value > RightValue ->
      leftRotate(Node);
    BalanceFactor > 1, Value < RightValue ->
      R = rightRotate(RightChild),
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

get(_, Tree) when length(Tree) == 0 ->
  undefined;
get(Value, [CV | Rest]) when Value == CV ->
  [CV] ++ Rest;
get(Value, [CV, _, LC, _]) when Value < CV ->
  get(Value, LC);
get(Value, [CV, _, _, RC]) when Value > CV ->
  get(Value, RC).

start() ->
  R1=add(20, []),
  R2=add(35, R1),
  R3=add(40, R2),
  R4=add(10, R3),
  R5=add(17, R4),
  R6=add(18, R5),
  R7=add(19, R6),
  R8=add(27, R7),
  R9=add(24, R8),
  R10=add(21, R9),
  R10,
  io:format("End").

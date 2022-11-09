-module(store_log).
-author("sbolsec").

%% API
-export([create/0, add/2, find/2, delete/2]).

create() ->
  [].

add(Store, Value) ->
  lists:append(Store, [Value]).

delete(Store, Value) ->
  lists:delete(Value, Store).

find(Store, Value) ->
  find_key(lists:filter(fun(X) -> X == Value end, Store)).

%%%%

find_key(FilteredList) when length(FilteredList) == 0 ->
  nil;
find_key([Value]) ->
  Value.

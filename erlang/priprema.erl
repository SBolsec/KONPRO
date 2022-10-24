-module(priprema).

-export([put/3, remove/2, get/2, update/3]).

put({Name, Surname}, [Friends], Map) ->
    maps:put({Name, Surname}, Friends, Map).

remove({Name, Surname}, Map) ->
    maps:remove({Name, Surname}, Map).

get({Name, Surname}, Map) ->
    maps:get({Name, Surname}, Map).

update({Name, Surname}, [Friends], Map) ->
    maps:update({Name, Surname}, Friends, Map).

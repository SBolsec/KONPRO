-module(second).

-export([loop/0]).

-import(priprema, [put/3, remove/2, get/2, update/3]).

% 02-1

loop() ->
    loop(#{}).

loop(Map) ->
    io:format("Mapa ~p~n", [Map]),
    receive 
        {put, {Name, Surname}, [Friends]} ->
            loop(priprema:put({Name, Surname}, [Friends], Map));
        {get, {Name, Surname}} ->
            Value = priprema:get({Name, Surname}, Map),
            io:format("GET found value ~w~n", [Value]),
            loop(Map);
        {remove, {Name, Surname}, Map} ->
            loop(priprema:remove({Name, Surname}, Map));
        {update, {Name, Surname}, [Friends], Map} ->
            loop(priprema:update({Name, Surname}, Friends, Map));
        stop -> 
            true
    end.

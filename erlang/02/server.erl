-module(server).

-export([start_server/0, put/2, get/1, remove/1, update/2, server_loop/0]).

% 02-2

start_server() ->
    Pid = spawn(server, server_loop, []),
    register(map_server, Pid).

server_loop() ->
    server_loop(#{}).

server_loop(Map) ->
    io:format("Mapa ~p~n", [Map]),
    receive
        {_, {put, {Name, Surname}, [Friends]}} ->
            server_loop(priprema:put({Name, Surname}, [Friends], Map));
        {From, {get, {Name, Surname}}} ->
            Value = priprema:get({Name, Surname}, Map),
            io:format("GET found value ~w~n", [Value]),
            From ! Value,
            server_loop(Map);
        {_, {remove, {Name, Surname}, Map}} ->
            server_loop(priprema:remove({Name, Surname}, Map));
        {_, {update, {Name, Surname}, [Friends], Map}} ->
            server_loop(priprema:update({Name, Surname}, Friends, Map));
        stop ->
            true
    end.

start_client({get, X}) ->
    map_server ! {self(), {get, X}},
    receive
        Result -> 
            io:format("Result ~w~n", [Result]), 
            Result
    end;
start_client(X) ->
    map_server ! {self(), X}.

put({Name, Surname}, [Friends]) ->
    start_client({put, {Name, Surname}, [Friends]}).

get({Name, Surname}) ->
    start_client({get, {Name, Surname}}).

remove({Name, Surname}) ->
    start_client({remove, {Name, Surname}}).

update({Name, Surname}, [Friends]) ->
    start_client({update, {Name, Surname}, [Friends]}).

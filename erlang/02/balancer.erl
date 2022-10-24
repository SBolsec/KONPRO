-module(balancer).

-export([start_server/0, put/2, get/1, remove/1, update/2, server_loop/0, main/2]).

% 02-3

start_server() ->
    Pid1 = spawn(balancer, server_loop, []),
    Pid2 = spawn(balancer, server_loop, []),
    Pid = spawn(balancer, main, [Pid1, Pid2]),
    register(main_server, Pid).

main(Pid1, Pid2) ->
    io:format("main~n", []),
    receive
        {From, {Action, {Name, Surname}}} ->
            if
                Name == pero -> Pid1 ! {From, {Action, {Name, Surname}}};       % staviti neki pravi uvjet za load balancing
                true -> Pid2 ! {From, {Action, {Name, Surname}}}
            end,
            main(Pid1, Pid2);
        {From, {Action, {Name, Surname}, Friends}} ->
            if
                Name == pero -> Pid1 ! {From, {Action, {Name, Surname}, Friends}};  % isto
                true -> Pid2 ! {From, {Action, {Name, Surname}, Friends}}
            end,
            main(Pid1, Pid2)
    end.

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
        {_, {remove, {Name, Surname}}} ->
            server_loop(priprema:remove({Name, Surname}, Map));
        {_, {update, {Name, Surname}, [Friends]}} ->
            server_loop(priprema:update({Name, Surname}, Friends, Map));
        stop ->
            true
    end.

start_client({get, X}) ->
    main_server ! {self(), {get, X}},
    receive
        Result ->
            io:format("Result ~w~n", [Result]),
            Result
    end;
start_client(X) ->
    main_server ! {self(), X}.

put({Name, Surname}, [Friends]) ->
    start_client({put, {Name, Surname}, [Friends]}).

get({Name, Surname}) ->
    start_client({get, {Name, Surname}}).

remove({Name, Surname}) ->
    start_client({remove, {Name, Surname}}).

update({Name, Surname}, [Friends]) ->
    start_client({update, {Name, Surname}, [Friends]}).

-module(balancer_serv).
-author("sbolsec").

-export([start_link/0, start_link/2]).
-export([add/1, delete/1, find/1, stop/0, stop_node/1]).
-export([main/2]).

start_link() ->
    start_link(number_serv, other_serv).

start_link(NumberServ, OtherServ) ->
    NumberServPid = spawn(store_serv, start_link, [OtherServ, false]),
    register(NumberServ, NumberServPid),
    OtherServerPid = spawn(store_serv, start_link, [NumberServ, false]),
    register(OtherServ, OtherServerPid),
    BalancerPid = spawn(balancer_serv, main, [NumberServ, OtherServ]),
    register(balancer, BalancerPid).

main(NumberServ, OtherServ) ->
    receive
        {_From, stop} ->
            NumberServ ! {self(), stop},
            OtherServ ! {self(), stop},
            stoped;
        {_From, {stop_node, ServName}} ->
            ServName ! {self(), stop},
            main(NumberServ, OtherServ);
        {From, {find, Value}} ->
            send({NumberServ, OtherServ}, {find, Value}),
            receive
                Result -> From ! Result
            end,
            main(NumberServ, OtherServ);
        {_From, {Action, Value}} ->
            send({NumberServ, OtherServ}, {Action, Value}),
            main(NumberServ, OtherServ)
    end.

send({NumberServ, OtherServ}, {Action, Value}) ->
    check_servers(NumberServ, OtherServ),
    if
        is_integer(Value) ->
            NumberServ ! {self(), {Action, Value}};
        true ->
            OtherServ ! {self(), {Action, Value}}
    end.

check_servers(NumberServ, OtherServ) ->
    check_server(NumberServ, OtherServ),
    check_server(OtherServ, NumberServ).

check_server(Master, Slave) ->
    io:format("Checking server ~w, process: ~w~n", [Master, self()]),
    Pid = whereis(Master),
    if
        Pid == undefined ->
            io:format("Server ~w is down, restarting it, process: ~w~n", [Master, self()]),
            NewPid = spawn(store_serv, start_link, [Slave, true]),
            register(Master, NewPid);
        true ->
            io:format("Server ~w is ok, process: ~w~n", [Master, self()]),
            ok
    end.

start_client(stop) ->
    balancer ! {self(), stop};
start_client({find, Value}) ->
    balancer ! {self(), {find, Value}},
    receive
        Result -> Result
    end;
start_client(X) ->
    balancer ! {self(), X}.

add(Value) ->
    start_client({add, Value}).

delete(Value) ->
    start_client({delete, Value}).

find(Value) ->
    start_client({find, Value}).

stop() ->
    start_client(stop).

stop_node(ServName) ->
    start_client({stop_node, ServName}).

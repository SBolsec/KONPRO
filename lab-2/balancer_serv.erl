-module(balancer_serv).
-author("sbolsec").

%% API
-export([start_link/0, start_link/2]).
-export([add/1, delete/1, find/1, stop/0]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link(number_serv, other_serv).

start_link(NumberServ, OtherServ) ->
    %% TODO: spawn servers
    NumberServPid = 1,
    OtherServerPid = 2,
    BalancerPid = spawn(balancer_serv, main, [NumberServPid, OtherServerPid]),
    register(balancer, BalancerPid).

main(NumberServPid, OtherServerPid) ->
    receive
        {From, {Action, Value}} ->
            1,
            main(NumberServPid, OtherServerPid)
    end.

start_client(stop) ->
    1;
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

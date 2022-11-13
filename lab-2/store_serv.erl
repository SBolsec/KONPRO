-module(store_serv).
-author("sbolsec").

%% API
-export([start_link/2]).

start_link(Master, Slave) ->
    1.

main() ->
    receive
        {From, {add, Value}} ->
            main();
        {From, {delete, Value}} ->
            main();
        {From, {find, Value}} ->
            main();
        {From, stop} ->
            stoped
    end.
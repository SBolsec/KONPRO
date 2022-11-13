-module(store_serv).
-author("sbolsec").

-export([start_link/2]).

start_link(Slave, false) ->
    io:format("Initializing process first time with slave ~w, process: ~w~n", [Slave, self()]),
    main([store_log:create(), store_log:create(), Slave]);
start_link(Slave, true) ->
    main(init(Slave)).

init(Slave) ->
    io:format("Initializing process with slave: ~w, process: ~w ~n", [Slave, self()]),
    Pid = whereis(Slave),
    if
        Pid == undefined ->
            io:format("Slave ~w is not running, creating initial empty state, process: ~w~n", [
                Slave, self()
            ]),
            [store_log:create(), store_log:create(), Slave];
        true ->
            io:format("Slave ~w is running, sending get_backup request, process: ~w~n", [
                Slave, self()
            ]),
            Slave ! {self(), get_backup},
            receive
                [MainStore, BackupStore] ->
                    io:format(
                        "Received backup from slave ~w, main store: ~w, backup store: ~w, process: ~w~n",
                        [Slave, MainStore, BackupStore, self()]
                    ),
                    [MainStore, BackupStore, Slave]
            end
    end.

main(State) ->
    receive
        {_From, {add, Value}} ->
            NewState = add(Value, State),
            io:format("Added value: ~w, new state: ~w, process: ~w ~n", [Value, NewState, self()]),
            main(NewState);
        {_From, {add_backup, Value}} ->
            NewState = add_backup(Value, State),
            io:format("Added backup value: ~w, new state: ~w, process: ~w ~n", [
                Value, NewState, self()
            ]),
            main(NewState);
        {_From, {delete, Value}} ->
            NewState = delete(Value, State),
            io:format("Deleted value: ~w, new state: ~w, process: ~w ~n", [Value, NewState, self()]),
            main(NewState);
        {_From, {delete_backup, Value}} ->
            NewState = delete_backup(Value, State),
            io:format("Deleted backup value: ~w, new state: ~w, process: ~w ~n", [
                Value, NewState, self()
            ]),
            main(NewState);
        {From, {find, Value}} ->
            Reply = find(Value, State),
            io:format("Find value: ~w, found: ~w, process: ~w ~n", [Value, Reply, self()]),
            From ! Reply,
            main(State);
        {From, get_backup} ->
            Reply = get_backup(State),
            io:format("Get backup, reply is: ~w, process: ~w ~n", [Reply, self()]),
            From ! Reply,
            main(State);
        {_From, stop} ->
            io:format("Stoping... ~w ~n", [self()]),
            stoped
    end.

add(Value, [MainStore, BackupStore, Slave]) ->
    NewMainStore = store_log:add(MainStore, Value),
    Slave ! {self(), {add_backup, Value}},
    [NewMainStore, BackupStore, Slave].

add_backup(Value, [MainStore, BackupStore, Slave]) ->
    NewBackupStore = store_log:add(BackupStore, Value),
    [MainStore, NewBackupStore, Slave].

delete(Value, [MainStore, BackupStore, Slave]) ->
    NewMainStore = store_log:delete(MainStore, Value),
    Slave ! {self(), {delete_backup, Value}},
    [NewMainStore, BackupStore, Slave].

delete_backup(Value, [MainStore, BackupStore, Slave]) ->
    NewBackupStore = store_log:delete(BackupStore, Value),
    [MainStore, NewBackupStore, Slave].

find(Value, [MainStore | _]) ->
    store_log:find(MainStore, Value).

get_backup([MainStore, BackupStore, _Slave]) ->
    [BackupStore, MainStore].

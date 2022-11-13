-module(store_serv).
-author("sbolsec").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Master, Slave) ->
    io:format("Node: Starting ~w, process: ~w~n", [Master, self()]),
    gen_server:start_link({local, Master}, ?MODULE, {Slave}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Slave}) ->
    io:format("Node: Init, trying to get backup from ~w, process: ~w~n", [Slave, self()]),
    try gen_server:call(Slave, get_backup) of
        [MainStore, BackupStore] ->
            io:format(
                "Node: Init, got backup from ~w, main store: ~w, backup store: ~w, process: ~w~n", [
                    Slave, MainStore, BackupStore, self()
                ]
            ),
            {ok, [MainStore, BackupStore, Slave]}
    catch
        exit:_ ->
            io:format(
                "Node: Init, failed to get backup from ~w, setting empty state, process: ~w~n", [
                    Slave, self()
                ]
            ),
            {ok, [store_log:create(), store_log:create(), Slave]}
    end.

handle_call(stop, _From, State) ->
    io:format("Node: Stopping process ~w~n", [self()]),
    {stop, normal, stopped, State};
handle_call({find, Value}, _From, State) ->
    io:format("Node: Find ~w, process: ~w~n", [Value, self()]),
    [MainStore | _] = State,
    Reply = store_log:find(MainStore, Value),
    io:format("Node: Found ~w, process: ~w~n", [Reply, self()]),
    {reply, Reply, State};
handle_call(get_backup, _From, State) ->
    io:format("Node: Get backup, process: ~w~n", [self()]),
    [MainStore, BackupStore, _] = State,
    io:format("Node: Backup reply: ~w, process: ~w~n", [[BackupStore, MainStore], self()]),
    {reply, [BackupStore, MainStore], State}.

handle_cast({add, Value}, [MainStore, _BackupStore, Slave]) ->
    io:format("Node: Add ~w, process: ~w~n", [Value, self()]),
    NewMainStore = store_log:add(MainStore, Value),
    io:format("Node: Added ~w, store: ~w, process: ~w~n", [Value, [NewMainStore, _BackupStore, Slave], self()]),
    io:format("Node: Add backup of ~w to slave ~w, process: ~w~n", [Value, Slave, self()]),
    gen_server:cast(Slave, {add_backup, Value}),
    {noreply, [NewMainStore, _BackupStore, Slave]};
handle_cast({add_backup, Value}, [_MainStore, BackupStore, _Slave]) ->
    io:format("Node: Add backup ~w, process: ~w~n", [Value, self()]),
    NewBackupStore = store_log:add(BackupStore, Value),
    io:format("Node: Added backup ~w, store: ~w, process: ~w~n", [Value, [_MainStore, NewBackupStore, _Slave], self()]),
    {noreply, [_MainStore, NewBackupStore, _Slave]};
handle_cast({delete, Value}, [MainStore, _BackupStore, Slave]) ->
    io:format("Node: Delete ~w, process: ~w~n", [Value, self()]),
    NewMainStore = store_log:delete(MainStore, Value),
    io:format("Node: Deleter ~w, store: ~w, process: ~w~n", [Value, [NewMainStore, _BackupStore, Slave], self()]),
    io:format("Node: Delete backup of ~w from slave ~w, process: ~w~n", [Value, Slave, self()]),
    gen_server:cast(Slave, {delete_backup, Value}),
    {noreply, [NewMainStore, _BackupStore, Slave]};
handle_cast({delete_backup, Value}, [_MainStore, BackupStore, _Slave]) ->
    io:format("Node: Delete backup ~w, process: ~w~n", [Value, self()]),
    NewBackupStore = store_log:delete(BackupStore, Value),
    io:format("Node: Deleted backup ~w, store: ~w, process: ~w~n", [Value, [_MainStore, NewBackupStore, _Slave], self()]),
    {noreply, [_MainStore, NewBackupStore, _Slave]};
handle_cast(debug, State) ->
    io:format("~w~n", [State]),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

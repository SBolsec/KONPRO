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
  gen_server:start_link({local, Master}, ?MODULE, {Slave}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Slave}) ->
  try gen_server:call(Slave, get_backup) of
    [MainStore, BackupStore] -> {ok, [MainStore, BackupStore, Slave]}
  catch
    exit:_ -> {ok, [store_log:create(), store_log:create(), Slave]}
  end.

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};
handle_call({find, Value}, _From, State) ->
  [MainStore | _] = State,
  Reply = store_log:find(MainStore, Value),
  {reply, Reply, State};
handle_call(get_backup, _From, State) ->
  [MainStore, BackupStore, _] = State,
  {reply, [BackupStore, MainStore], State}.

handle_cast({add, Value}, [MainStore, _BackupStore, Slave]) ->
  NewMainStore = store_log:add(MainStore, Value),
  gen_server:cast(Slave, {add_backup, Value}),
  {noreply, [NewMainStore, _BackupStore, Slave]};
handle_cast({add_backup, Value}, [_MainStore, BackupStore, _Slave]) ->
  NewBackupStore = store_log:add(BackupStore, Value),
  {noreply, [_MainStore, NewBackupStore, _Slave]};
handle_cast({delete, Value}, [MainStore, _BackupStore, Slave]) ->
  NewMainStore = store_log:delete(MainStore, Value),
  gen_server:cast(Slave, {delete_backup, Value}),
  {noreply, [NewMainStore, _BackupStore, Slave]};
handle_cast({delete_backup, Value}, [_MainStore, BackupStore, _Slave]) ->
  NewBackupStore = store_log:delete(BackupStore, Value),
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

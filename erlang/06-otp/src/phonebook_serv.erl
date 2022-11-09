-module(phonebook_serv).
-author("sbolsec").

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, add/2, find/1, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:call(?SERVER, stop).

add(Name, Number) ->
  gen_server:cast(?SERVER, {add, Name, Number}).

find(Name) ->
  gen_server:call(?SERVER, {find, Name}).

delete(Name) ->
  gen_server:cast(?SERVER, {delete, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
  {ok, phonebook_log:create()}.

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};
handle_call({find, Name}, _From, State) ->
  Reply = phonebook_log:find(State, Name),
  {reply, Reply, State}.

handle_cast({add, Name, Number}, State) ->
  NewState = phonebook_log:add(State, {Name, Number}),
  {noreply, NewState};
handle_cast({delete, Name}, State) ->
  NewState = phonebook_log:delete(State, Name),
  {noreply, NewState}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

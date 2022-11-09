-module(balancer_serv).
-author("sbolsec").

-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([add/1, delete/1, find/1, stop/0, debug/1, stop_node/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link(NumberServ, OtherServ) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {NumberServ, OtherServ}, []).

stop() ->
  gen_server:call(?SERVER, stop).

stop_node(ServName) ->
  gen_server:cast(?SERVER, {stop_node, ServName}).

add(Value) ->
  gen_server:cast(?SERVER, {add, Value}).

delete(Value) ->
  gen_server:cast(?SERVER, {delete, Value}).

find(Value) ->
  gen_server:call(?SERVER, {find, Value}).

debug(ServName) ->
  gen_server:cast(?SERVER, {debug, ServName}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({NumberServ, OtherServ}) ->
  {ok, {NumberServ, OtherServ}}.

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};
handle_call({find, Value}, _From, {NumberServ, OtherServ}) when is_integer(Value) ->
  Reply = gen_server:call(NumberServ, {find, Value}),
  {reply, Reply, {NumberServ, OtherServ}};
handle_call({find, Value}, _From, {NumberServ, OtherServ}) ->
  Reply = gen_server:call(OtherServ, {find, Value}),
  {reply, Reply, {NumberServ, OtherServ}}.

handle_cast({add, Value}, {NumberServ, OtherServ}) when is_integer(Value) ->
  gen_server:cast(NumberServ, {add, Value}),
  {noreply, {NumberServ, OtherServ}};
handle_cast({add, Value}, {NumberServ, OtherServ}) ->
  gen_server:cast(OtherServ, {add, Value}),
  {noreply, {NumberServ, OtherServ}};
handle_cast({delete, Value}, {NumberServ, OtherServ}) when is_integer(Value) ->
  gen_server:cast(NumberServ, {add, Value}),
  {noreply, {NumberServ, OtherServ}};
handle_cast({delete, Value}, {NumberServ, OtherServ}) ->
  gen_server:cast(OtherServ, {add, Value}),
  {noreply, {NumberServ, OtherServ}};
handle_cast({debug, ServName}, {NumberServ, OtherServ}) when ServName == NumberServ ->
  gen_server:cast(NumberServ, debug),
  {noreply, {NumberServ, OtherServ}};
handle_cast({debug, ServName}, {NumberServ, OtherServ}) when ServName == OtherServ ->
  gen_server:cast(OtherServ, debug),
  {noreply, {NumberServ, OtherServ}};
handle_cast({stop_node, ServName}, {NumberServ, OtherServ}) when ServName == NumberServ ->
  gen_server:cast(NumberServ, stop),
  {noreply, {NumberServ, OtherServ}};
handle_cast({stop_node, ServName}, {NumberServ, OtherServ}) when ServName == OtherServ ->
  gen_server:cast(OtherServ, stop),
  {noreply, {NumberServ, OtherServ}}.


handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

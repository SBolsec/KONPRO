-module(store_sup).
-author("sbolsec").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  RestartStrategy = one_for_one,
  MaxRestarts = 3,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 2000,
  Type = worker,

  BalanceName = balancer_serv,
  NumberName = number_serv,
  OtherName = other_serv,

  BalancerServ = {BalanceName, {BalanceName, start_link, [NumberName, OtherName]}, Restart, Shutdown, Type, [balancer_serv]},
  NumberServ = {NumberName, {store_serv, start_link, [NumberName, OtherName]}, Restart, Shutdown, Type, [store_serv]},
  OtherServ = {OtherName, {store_serv, start_link, [OtherName, NumberName]}, Restart, Shutdown, Type, [store_serv]},

  {ok, {SupFlags, [BalancerServ, NumberServ, OtherServ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-module(mi_serv).
-author("sbolsec").

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, add_missing_primes/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

stop() ->
  gen_server:call(?SERVER, stop).

add_missing_primes(List) ->
    gen_server:call(?SERVER, {add_missing_primes, List}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_) ->
    {ok, {}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};
handle_call({add_missing_primes, List}, _From, State) ->
    {reply, mi:add_missing_primes(List), State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

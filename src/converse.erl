-module (converse).
-include ("converse.hrl").

-behaviour(application).
-define (APPLICATIONS_TO_START, []).
 
%% application callbacks
-export([start/2, stop/1]).
-export ([init/1]).
-export ([send_message/1]).

send_message(Msg) -> converse_tcp:send_message(Msg).

start(_Type, Config) ->    
    layers:start_bundle([
      {"Applications", fun() -> [application:start(A) || A <- ?APPLICATIONS_TO_START] end},
      {"Converse supervisor", fun() -> converse_sup:start_link() end},
      {"Converse listener", fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]) end}
    ]).

init([Config]) ->
  TcpServerSup = { converse_tcp, {converse_tcp,start_link,[Config]}, permanent,2000,worker,[]},
  {ok,
   {_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
        [
        TcpServerSup
        ]}
  }.

stop(State) -> ok.
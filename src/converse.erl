-module (converse).
-include ("converse.hrl").

-behaviour(application).
-define (APPLICATIONS_TO_START, []).
 
%% application callbacks
-export([start/2, stop/1]).
-export ([init/1]).
-export ([send_message/2, send_message/3]).

send_message(Port, Msg) -> send_message("0.0.0.0", Port, Msg).
  
send_message(Addr, Port, Msg) -> 
  Socket = converse_udp:send(Msg, Addr, Port),
  gen_udp:recv(Socket, 4096, 10000),
  receive
    {ok, {_,_,Reply}} -> Reply;
    Error -> no_reply
  after
    500 -> no_reply
  end.  
  
cast_message(Addr, Msg) -> converse_upd:cast_message(Addr, Msg).
reply(Socket, Msg) -> converse_tcp:reply_message(Socket, Msg).

start(_Type, Config) ->    
    layers:start_bundle([
      {"Applications", fun() -> [application:start(A) || A <- ?APPLICATIONS_TO_START] end},
      {"Converse logger", fun() -> converse_logger:start_link() end},
      {"Converse listener", fun() -> 
        [Port] = config:fetch_or_default_config([port], Config, ?DEFAULT_CONFIG),
        ServerName = converse_utils:registered_name(?MODULE, integer_to_list(Port)),
        supervisor:start_link(?MODULE, [ServerName, Config]) 
      end}
    ]).

init([ServerName, Config]) ->
  TcpServerSup = { converser, {converse_udp,start_named,[ServerName, Config]}, permanent,2000,worker,[]},
  {ok,{_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},[TcpServerSup]}}.

stop(State) -> ok.

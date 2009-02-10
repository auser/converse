-module (converse).

-revision ("Revision 0.3").
-include ("converse.hrl").

-behaviour(application).

%% Internal API
-export([start_tcp_client/1, start_udp_client/1]).
-export ([open_and_send/2, send_to_open/2]).
-export ([echo/1]).
%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

%% API
% start_link(Config) ->
% 	converse:start(normal, Config).
	
open_and_send({Address, Port}, Data) ->
	case converse_listener:open_socket({Address, Port}) of
		{ok, Socket} ->
			case send_to_open(Socket, {keyreq}) of
				{error, Reason} ->
					io:format("Error in initiating call: ~p~n", [Reason]),
					{error, Reason};
				_Anything ->
					send_to_open(Socket, Data)
			end;
		{error, Reason} -> {error, Reason}
	end.

echo({Address, Port}) ->
	case converse_listener:open_socket({Address, Port}) of
		{ok, Socket} ->			
			send_to_open(Socket, {echo, "echo"});
		{error, Reason} -> {error, Reason}
	end.

send_to_open(Socket, Data) ->
	gen_tcp:send(Socket, converse_packet:encode(Data)),
	{ok, Socket}.

start_udp_client(Fun) ->
	supervisor:start_child(udp_client_sup, []).
	
start_tcp_client(Config) -> 
	_ChildSpec = [{undefined,{tcp_app_fsm,start_link,[Config]},temporary,2000,worker,[]}],
	supervisor:start_child(tcp_client_sup, []).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, Config) ->
		application:start(crypto),
		supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).

stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Config]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
            [
              % TCP Listener
              {   tcp_server,                          % Id       = internal id
                  {converse_listener,start_link,[Config]}, % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [converse_listener]                           % Modules  = [Module] | dynamic
              }
              % Client instance supervisor
              ,{  tcp_client,
                  {supervisor,start_link,[{local, tcp_client_sup}, ?MODULE, [tcp_app_fsm, Config]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
              % ,{  udp_client,
              %     {supervisor,start_link,[{local, udp_client_sup}, ?MODULE, [udp_app_fsm, Config]]},
              %     permanent,                               % Restart  = permanent | transient | temporary
              %     infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
              %     supervisor,                              % Type     = worker | supervisor
              %     []                                       % Modules  = [Module] | dynamic
              % }
            ]
        }
    };

init([Module, Config]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[Config]},   				 % StartFun = {M, F, A}
                  temporary,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
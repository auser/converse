-module (tcp_server_app).
-include ("converse.hrl").
-behaviour(application).

%% Internal API
-export([start_client/0]).

%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
start_client() -> supervisor:start_child(tcp_client_sup, []).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, Args) ->
		DefaultPort = config:parse(port, ?DEFAULT_CONFIG),
    Port = utils:get_app_env(listen_port, DefaultPort),
    supervisor:start_link(?MODULE, [Port, tcp_app_fsm, Args]). % {local, ?MODULE}

stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Port, Module, ReceiveFunction]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
            [
              % TCP Listener
              {   tcp_server_sup,                          % Id       = internal id
                  {tcp_listener,start_link,[Port,Module]}, % StartFun = {M, F, A}
                  permanent,                               % Restart  = permanent | transient | temporary
                  2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                                  % Type     = worker | supervisor
                  [tcp_listener]                           % Modules  = [Module] | dynamic
              },
              % Client instance supervisor
              {   tcp_client_sup,
                  {supervisor,start_link,[{local, tcp_client_sup}, ?MODULE, [Module, ReceiveFunction]]},
                  permanent,                               % Restart  = permanent | transient | temporary
                  infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,                              % Type     = worker | supervisor
                  []                                       % Modules  = [Module] | dynamic
              }
            ]
        }
    };

init([Module, ReceiveFunction]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
            [
              % TCP Client
              {   undefined,                               % Id       = internal id
                  {Module,start_link,[ReceiveFunction]},   % StartFun = {M, F, A}
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
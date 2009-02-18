-module (converse_listener_sup).
-include ("converse.hrl").

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

start_link(Name, Config) ->
	?TRACE("Config in converse_listener_sup", [Config]),
	supervisor:start_link({local, Name}, ?MODULE, [Config]).

init([Config]) ->		
	?TRACE("Config in converse_listener_sup", [Config]),
	{ok,
	    {_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
	        [
	          % TCP Listener
	          { tcp_server, % Id = internal id
	              {converse_listener,start_link,[Config]}, % StartFun = {M, F, A}
	              permanent, % Restart = permanent | transient | temporary
	              2000, % Shutdown = brutal_kill | int() >= 0 | infinity
	              worker, % Type = worker | supervisor
	              [converse_listener] % Modules = [Module] | dynamic
	          }
	          % Client instance supervisor
	          ,{ tcp_client,
	              {supervisor,start_link,[{local, tcp_client_sup}, ?MODULE, [tcp_app_fsm, Config]]},
	              permanent, % Restart = permanent | transient | temporary
	              infinity, % Shutdown = brutal_kill | int() >= 0 | infinity
	              supervisor, % Type = worker | supervisor
	              [] % Modules = [Module] | dynamic
	          }
	          % ,{ udp_client,
	          % {supervisor,start_link,[{local, udp_client_sup}, ?MODULE, [udp_app_fsm, Config]]},
	          % permanent, % Restart = permanent | transient | temporary
	          % infinity, % Shutdown = brutal_kill | int() >= 0 | infinity
	          % supervisor, % Type = worker | supervisor
	          % [] % Modules = [Module] | dynamic
	          % }
	        ]
	    }
	};

init([Module, Config]) ->
	{ok,
	    {_SupFlags = {simple_one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
	        [
	          % TCP Client
	          { undefined, % Id = internal id
	              {Module,start_link,[Config]},          % StartFun = {M, F, A}
	              temporary, % Restart = permanent | transient | temporary
	              2000, % Shutdown = brutal_kill | int() >= 0 | infinity
	              worker, % Type = worker | supervisor
	              [] % Modules = [Module] | dynamic
	          }
	        ]
	    }
	}.

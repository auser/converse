-module (tcp_fsm_sup).
-include ("converse.hrl").

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

start_link(Name, Config) ->
    supervisor:start_link({local,Name}, ?MODULE, Config).

init([Config]) ->
	{ok, {{one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
	      [{tcp_app_fsm, {tcp_app_fsm, start_link, [Config]},
	        transient, brutal_kill, worker, [tcp_app_fsm]}]}}.
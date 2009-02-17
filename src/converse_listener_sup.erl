-module (converse_listener_sup).
-include ("converse.hrl").

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

start_link(Name, Config) ->
    supervisor:start_link({local,Name}, ?MODULE, Config).

init([Config]) ->
	{ok, {{one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
	      [{converse_listener, {converse_listener, start_link, [Config]},
	        permanent, brutal_kill, worker, [converse_listener]}]}}.
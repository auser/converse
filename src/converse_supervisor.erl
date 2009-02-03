-module (converse_supervisor).
-include("converse.hrl").

-behaviour(supervisor).

-export([start_link/0, start_link/1, start_link/2, init/1]).
		
start_link() ->
	start_link(?DEFAULT_CONFIG, undefined).
	
start_link( Fun ) ->
	Config = ?DEFAULT_CONFIG,
	start_link( Config, Fun ).

start_link(Config, Fun) ->
	supervisor:start_link(?MODULE, [Config, Fun]).

init([Config, Fun]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxTimeBetRestarts = 3600,
	TimeoutTime = 5000,
	
	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
	
	LoadServers = [
		{converse_tcp, {converse_tcp, start_link, [Config, Fun]}, 
			permanent, TimeoutTime, worker, 
			[converse_tcp]}
	],
	
	{ok, {SupFlags, LoadServers}}.
    
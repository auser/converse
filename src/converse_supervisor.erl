-module (converse_supervisor).
-include("converse.hrl").

-behaviour(supervisor).

-export([start_in_shell_for_testing/2, start_link/2, init/1]).

start_in_shell_for_testing(Port, Starter) ->
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, Starter]),
	unlink(Pid).

start_link(Port, Starter) ->
	spawn(fun() ->
			supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, Starter])
		end).

init([Port, Starter]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxTimeBetRestarts = 3600,
	TimeoutTime = 5000,
	
	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
	
    TcpListener = {converse_tcp_listener, 
		{converse_tcp_listener, start_link, [Port, Starter]}, 
		permanent, 
		TimeoutTime, 
		worker, 
		[converse_tcp_listener]},

    Connector = {converse_connector, 
		{converse_connector, start_link, []}, 
		permanent,
		TimeoutTime,
		worker, 
		[converse_connector]},

    LoadServers = [TcpListener,Connector],

	{ok, {SupFlags, LoadServers}}.
    
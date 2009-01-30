-module (talker_supervisor).

-behaviour(supervisor).

-export([start_in_shell_for_testing/0, start_link/0, init/1]).

start_in_shell_for_testing() ->
	{ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []),
	unlink(Pid).

start_link() ->
	spawn(fun() ->
			supervisor:start_link({local, ?MODULE}, ?MODULE, _Args = [])
		end).

init([]) ->
	RestartStrategy = one_for_one,
	MaxRestarts = 1000,
	MaxTimeBetRestarts = 3600,
	TimeoutTime = 5000,
	
	SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},
	
    TalkRouter = {talker_router, 
		{talker_router, start_link, []}, 
		permanent, 
		TimeoutTime, 
		worker, 
		[talker_router]},

    LoadServers = [TalkRouter],

	{ok, {SupFlags, LoadServers}}.
    
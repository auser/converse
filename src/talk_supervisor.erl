-module (talk_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
	InstanceId = random:uniform(),
    Talk_router =
	{talk_router,
	 {talk_router, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 []},
    Talker =
	{talker,
	 {talker, start_link, [InstanceId]},
	 permanent,
	 brutal_kill,
	 worker,
	 []},
    {ok, {{one_for_all, 10, 1},
	  [
		Talk_router,
		Talker
	  ]}}.
    
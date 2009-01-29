-module (talker_supervisor).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    Talk_router =
	{talker_router,
	 {talker_router, start_link, []},
	 permanent,
	 brutal_kill,
	 worker,
	 []},
	%     Talker =
	% {talker,
	%  {talker, start_link, []},
	%  permanent,
	%  brutal_kill,
	%  worker,
	%  []},
    {ok, {{one_for_all, 10, 1},
	  [
		Talk_router
		% Talker
	  ]}}.
    
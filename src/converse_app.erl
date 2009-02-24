-module (converse_app).
-include ("converse_app.hrl").

-behaviour(application).
-define (APPLICATIONS_TO_START, []).

%% application callbacks
-export([start/2, stop/1]).
-export ([start_tcp_client/2]).
-export ([init/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

start_tcp_client(Name, Config) -> 	
	% supervisor:start_child(converse_tcp, [Name]).
	supervisor:start_link({local, Name}, converse_tcp, [Name, Config]),
	global:whereis_name(Name).
	
%%----------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%%----------------------------------------------------------------------
start(_Type, Config) ->		
		layers:start_bundle([
			{"Applications", fun() -> [application:start(A) || A <- ?APPLICATIONS_TO_START] end},
			{"Converse supervisor", fun() -> converse_sup:start_link() end},
			{"Converse listener", fun() -> 
				% converse_tcp:start_link(Config),
				supervisor:start_link({local, ?MODULE}, ?MODULE, [converse_tcp, Config])
				% start_tcp_client(Config)
				% start_child(tcp_app_fsm, Config),
				end}
		]).

%%----------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%----------------------------------------------------------------------
stop(State) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Module, Config]) ->		
	{ok,
	    {_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
				[{ tcp_server,
				    {converse_listener,start_link,[Config]},
				    permanent,2000,worker,[converse_listener]
				},{ tcp_client,
				    {supervisor,start_link,[{local, converse_tcp}, ?MODULE, [sup, Module, Config]]},
				    permanent,infinity,supervisor,[]
				}]}
	};

% Client supervisor
init([sup, Module, Config]) ->
	{ok,
		{_SupFlags = {simple_one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
		[{ undefined, {Module,start_link,[Config]},permanent,2000,worker,[]}]}
	}.

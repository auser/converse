-module (converse_app).
-include ("converse_app.hrl").

-behaviour(application).
-define (APPLICATIONS_TO_START, []).

%% application callbacks
-export([start/2, stop/1]).
-export ([start_client/1]).
-export ([init/1]).

%%%----------------------------------------------------------------------
%%% Callback functions from application
%%%----------------------------------------------------------------------

start_client(Config) ->
	supervisor:start_child(tcp_client_sup, []).
	
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
			{"Converse listener", fun() -> supervisor:start_link({local, ?MODULE}, ?MODULE, [converse_tcp, Config]) end}
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
	TcpServerSup = { tcp_server, {converse_listener,start_link,[converse_tcp, Config]}, permanent,2000,worker,[converse_listener]},
	{ok,
	    {_SupFlags = {one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
				[TcpServerSup,
				{ tcp_client_sup, {supervisor,start_link,[{local, tcp_client_sup}, ?MODULE, [sup, Module, Config]]}, permanent,infinity,supervisor,[]}
				]}
	};

% Client supervisor
init([sup, Module, Config]) ->
	{ok,
		{_SupFlags = {simple_one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
		[{ undefined, {Module,start_link,[Config]},temporary,2000,worker,[]}]}
	}.

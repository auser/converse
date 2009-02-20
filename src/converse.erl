-module (converse).
-include ("converse.hrl").

-behaviour(application).

%% Internal API
-export([start_tcp_client/1, start_udp_client/1]).
-export ([send/3, open_and_send/3, send_to_open/2]).
-export ([layers_receive/0]).
%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

-import (io_lib).
%% API

% Only used for testing
layers_receive() ->
	receive
		Anything -> 
			io:format("Received (in ~p) ~p~n", [?MODULE, Anything]),
			layers_receive()
	end.

start_udp_client(Fun) ->
	supervisor:start_child(udp_client_sup, []).
	
start_tcp_client(Config) -> 	
	% Ip = tcp_host({0,0,0,0}),
	Port = list_to_atom(integer_to_list(config:parse(port, Config))),
	% Name = erlang:list_to_atom(lists:flatten(io_lib:format("~p_~p_~p", [converse_listener, Ip, Port]))),
	?TRACE("Starting tcp client on port", [Port]),
	supervisor:start_child(tcp_client_sup, []).
	% tcp_fsm_sup:start_link(Name, Config).
	% supervisor:start_child(tcp_client_sup, ChildSpec).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, Config) ->		
		layers:start_bundle([
			{"Applications", fun() -> [application:start(A) || A <- ?APPLICATIONS_TO_START] end},
			{"Converse supervisor", fun() -> converse_sup:start_link() end},
			{"Converse listener", fun() -> 
				supervisor:start_link({local, ?MODULE}, ?MODULE, [tcp_app_fsm, Config]) 
				% start_child(tcp_app_fsm, Config),
				end}
		]),
		Fun = config:parse(successor, Config),
		% layers:register_process(Fun, whereis(converse_listener)),
		ok.

stop(_S) ->
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
				    {supervisor,start_link,[{local, tcp_client_sup}, ?MODULE, [sup, Module, Config]]},
				    permanent,infinity,supervisor,[]
				}]}
	};

init([sup, Module, Config]) ->
	{ok,
	    {_SupFlags = {simple_one_for_one, ?MAXIMUM_RESTARTS, ?MAX_DELAY_TIME},
	        [
	          % TCP Client
	          { undefined, % Id = internal id
	              {Module,start_link,[Config]},          % StartFun = {M, F, A}
	              permanent, % Restart = permanent | transient | temporary
	              2000, % Shutdown = brutal_kill | int() >= 0 | infinity
	              worker, % Type = worker | supervisor
	              [] % Modules = [Module] | dynamic
	          }
	        ]
	    }
	}.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
send(Address, Port, Msg) -> open_and_send(Address, Port, Msg).

open_and_send(Address, Port, Msg) ->
	case open_socket({Address, Port}) of
		{ok, Socket} ->
			send_to_open(Socket, Msg),
			{ok, Socket};
		{error, Reason} ->
			io:format("Error in sending message to ~p:~p: ~p~n", [Address, Port, Reason]),
			error
	end.

start(Type, StartArgs) -> converse_app:start(Type, StartArgs).

open_socket({Address, Port}) ->
	case gen_tcp:connect(Address, Port, [{packet, raw}]) of
		{error, Reason} ->
			io:format("Error ~p~n", [Reason]),
			{error, Reason};
		{ok, Socket} ->
			{ok, Socket}
	end.
	
tcp_host({0,0,0,0}) ->
    {ok, Hostname} = inet:gethostname(),
    case inet:gethostbyname(Hostname) of
        {ok, #hostent{h_name = Name}} -> Name;
        {error, _Reason} -> Hostname
    end;
tcp_host(IPAddress) ->
    case inet:gethostbyaddr(IPAddress) of
        {ok, #hostent{h_name = Name}} -> Name;
        {error, _Reason} -> inet_parse:ntoa(IPAddress)
    end.

start_child(Mod, Args) ->
    {ok,_} = supervisor:start_child(converse_sup,
                                    {Mod, {Mod, start_link, [Args]},
                                     transient, 100, worker, [Mod]}),
    ok.

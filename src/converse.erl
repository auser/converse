-module (converse).

-revision ("Revision 0.3").
-include ("converse.hrl").

-behaviour(application).

%% Internal API
-export([start_tcp_client/1, start_udp_client/1]).
-export ([send/3, open_and_send/3, send_to_open/2]).
-export ([layers_receive/1]).
%% Application and Supervisor callbacks
-export([start/2, stop/1, init/1]).

-import (io_lib).
%% API

% Only used for testing
layers_receive(From) ->
	receive
		Anything -> 
			io:format("Received (in ~p) ~p~n", [?MODULE, Anything]),
			layers_receive(From)
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
				{"Converse listener", fun() -> converse_listener_sup:start_link(?MODULE, Config) end}
			]).

stop(_S) ->
    ok.

%%----------------------------------------------------------------------
%% Supervisor behaviour callbacks
%%----------------------------------------------------------------------
init([Config]) ->
	Fun = config:parse(successor, Config),
	layers:register_process(Fun, self()),
	{ok, []}.

%%----------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------
send(Address, Port, Msg) -> open_and_send(Address, Port, Msg).

open_and_send(Address, Port, Msg) ->
	case open_socket({Address, Port}) of
		{ok, Socket} ->
			send_to_open(Socket, Msg),
			ok;
		{error, Reason} ->
			io:format("Error in sending message to ~p:~p: ~p~n", [Address, Port, Reason]),
			error
	end.

send_to_open(Socket, Msg) -> gen_tcp:send(Socket, converse_packet:encode(Msg)).

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

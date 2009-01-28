-module (talker).
-export ([start_link/1, init/2]).

-import(inet).
-include_lib("kernel/include/inet.hrl").
-include("talker.hrl").

start_link(Id) ->
	io:format("Starting ~p talker server~n", [Id]),
	Pid = spawn_link(talker, init, [Id, self()]),
	receive
		{started} ->
			{ok, Pid}
	end.

init(_Id, Super) ->
	erlang:register(?MODULE, self()),
	io:format("Registered ~p with erlang~n", [self()]),
	ListeningSocket = open_port_for_listening(5001, my_ip()),
	io:format("ListeningSocket is set as ~p~n", [ListeningSocket]),
	{ok, {_LAddress, LPort}} = inet:sockname(ListeningSocket),
	io:format("LPort = ~p~n", [LPort]),
	talker_router:set_local_address(undefined, LPort),
	Super ! {started},
	server(ListeningSocket).

server(Sock) ->
	io:format("Starting server on ~p~n", [Sock]),
	case gen_tcp:accept(Sock) of
		{undefined, LPort} ->
			{ok, {MyIp, _}} = inet:sockname(Sock),
			talker_router:set_local_address(MyIp, LPort),
			io:format("Set my address as ~p on ~p in talker_router~n", [MyIp, LPort]);
		_ ->
			ok
	end,
	receive
		{tcp, S, Message} ->
			{endpoint, Add, Port} = binary_to_term(Message),
			NewAddress = if Add =:= {0,0,0,0} orelse Add =:= {127,0,0,1} ->
				case inet:peername(S) of
					{ok, {PeerAdd, _Port}} ->
						PeerAdd;
					{error, _Reason} ->
						Add
				end;
			true ->
				Add
			end,
			NewPid = talk_sender:new(NewAddress, Port, S),
			gen_tcp:controlling_process(S, NewPid),
			inet:setopts(S, [{active, once}]),
			talker_router:add_connection(NewAddress, Port, NewPid, S),
			server(Sock);
	_Other ->
		ok
	end.

open_port_for_listening(Port, Ip) ->
	case gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, 
					      {active, once}, {ip, Ip}]) of
	{ok, Socket} ->
	    Socket;
	{error, Reason} ->
	    io:format("can't listen on ~p: ~p~n", [Port, Reason])
    end.

my_ip() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, HostEntry} = inet:gethostbyname(Hostname),
    erlang:hd(HostEntry#hostent.h_addr_list).
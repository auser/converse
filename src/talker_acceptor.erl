-module (talker_acceptor).
-include("talker.hrl").

-export ([start_acceptor/2, init/2]).

start_acceptor(Port, Ip) ->
	io:format("Starting talker_acceptor with ~p, ~p~n", [Port, Ip]),
	case open_socket_and_start_listening(Port, Ip) of
		{started, Socket, Pid} -> 
			{connection, Pid, Socket};
		Else ->
			io:format("Error when starting acceptor: ~p~n", [Else]),
			{error, Else}
	end.
	
open_socket_and_start_listening(Port, Ip) ->
	case talker_connection:open_port_for_listening(Port, Ip) of
		{stop, Reason} ->
			io:format("Error starting talk_router: ~p~n", [Reason]),
			{stop, Reason};
		Socket ->
			{ok, {_LAddress, LPort}} = inet:sockname(Socket),
			Pid = proc_lib:spawn_link(?MODULE, init, [LPort, Socket]),
			io:format("Started with ~p with ~p~n", [LPort, Socket]),
			{started, Socket, Pid}
	end.
	
	
init(Port, Socket) ->
	case gen_tcp:accept(Socket) of
		{undefined, Port} ->
			{ok, {Ip, _}} = inet:sockname(Socket),
			talker_router:set_local_address(Ip, Port),
			io:format("Set my address as ~p on ~p in talker_router~n", [Ip, Port]);
		_ ->
			ok
	end,
	server(Socket).

server(Socket) ->
	receive
		{tcp, Socket, Data} ->
			{server, Address, Port} = binary_to_term(Data),
			NewPid = talker:send({Address, Port}, Data),
			gen_tcp:controlling_process(Socket, NewPid),
			inet:setopts(Socket, [{active, once}]),
			talker_router:register_connection(Address, Port, NewPid, Socket),
			server(Socket);
		Else ->
			io:format("Received ~p from the server socket", [Else]),
			ok
	end.
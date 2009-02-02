-module (converse_tcp_listener).
-include ("converse.hrl").

-export ([start_link/2, init/3]).

start_link(Port, Starter) ->
	Pid = proc_lib:spawn_link(?MODULE, init, [Port, self(), Starter]),
	receive
		{started, _ListeningPort} ->
			{ok, Pid};
		{error, Error} ->
			io:format("Error in start_link ~p~n", [Error]),
			ok
	end.
	
init(Port, Supervisor, Starter) ->
	% Open a socket for listening on the port at the first available Ip address
	IntegerPort = case erlang:is_atom(Port) of
		true -> 
			erlang:list_to_integer(erlang:atom_to_list(Port));
		false -> 
			Port
	end,
	Socket = open_port_for_listening(IntegerPort),
	% notify self that we've started so we can move on from start_link
	{ok, ListeningPort} = inet:port(Socket),
	Supervisor ! {started, ListeningPort},
	% Start the server process
	server(Socket, Starter).

% Just start the server with the starter to be notified
server(LS, Starter) ->
	% Accept connections on the listening socket
    case gen_tcp:accept(LS) of
	% If the socket is ready to listen
	{ok, Socket} ->
		% then spawn a server to listen to it, maintaining the same starter pid
		% spawn(fun() -> 
			listen_loop(Socket, Starter),
			% end),
	    server(LS, Starter);
	Other ->
		% otherwise, wha huh?
		io:format("Unknown message: ~p~n", [Other]),
		server(LS, Starter)
    end.

listen_loop(LS, Starter) ->
	inet:setopts(LS, [{active, once}]),
	receive
		{tcp, Socket, Data} ->
			case binary_to_term(Data) of
				{deliver, Message} ->					
					Response = handle_deliver_message(Message, Starter),
					io:format("Received message ~p -> ~p (to ~p)~n", [Message, Response, Starter]),
					listen_loop(Socket, Starter);
				{server, _Address, _Port} ->
					io:format("Received notification that I'll be receiving stuff~n"),
					listen_loop(Socket, Starter);
				{remote, _Address, _Port} ->
					io:format("Received notification that I'll be receiving stuff from remote~n"),
					listen_loop(Socket, Starter);
				{close} ->
					gen_tcp:close(Socket);
				Other ->
					io:format("Received ~p message from accept~n", [Other]),
					listen_loop(Socket, Starter)
			end;
		{tcp_closed,S} ->
			io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok;
		Other ->
			io:format("Received ~p~n", [Other]),
			listen_loop(LS, Starter)
	end.

open_port_for_listening(Port) ->
	case gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, 
					      {active, once}]) of
	{ok, Socket} ->
	    Socket;
	Else ->
	    io:format("Error: can't listen on ~p: ~p~n", [Port, Else]),
		{stop, Else}
    end.

% first_available_ip() ->
%     {ok, Hostname} = inet:gethostname(),
%     {ok, HostEntry} = inet:gethostbyname(Hostname),
%     erlang:hd(HostEntry#hostent.h_addr_list).

handle_deliver_message(Message, Starter) ->
	Starter ! Message.
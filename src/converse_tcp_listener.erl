-module (converse_tcp_listener).
-include ("converse.hrl").

-export ([start_link/2, init/3]).

start_link(Port, StarterName) ->
	Pid = proc_lib:spawn_link(?MODULE, init, [Port, self(), StarterName]),
	receive
		{started} ->
			{ok, Pid};
		{error, Error} ->
			?TRACE("Error in start_link ~p~n", [Error]),
			fail
	end.
	
init(Port, Supervisor, StarterName) ->	
	process_flag(trap_exit, true),
	% Open a socket for listening on the port at the first available Ip address
	IntegerPort = utils:safe_integer(Port),
	Socket = open_port_for_listening(IntegerPort),
	% notify self that we've started so we can move on from start_link
	Supervisor ! {started},
	% Start the server process
	?TRACE("Starting the server process~n", []),	
	server(Socket, StarterName).

% Just start the server with the starter to be notified
server(LS, StarterName) ->
	% Accept connections on the listening socket
    case gen_tcp:accept(LS) of
	% If the socket is ready to listen
	{ok, Socket} ->
		?TRACE("Starting server", [StarterName]),
		% then spawn a server to listen to it, maintaining the same starter pid
		% spawn(fun() -> 
			listen_loop(Socket, StarterName),
			% end),
	    server(LS, StarterName);
	{error, closed} ->
		?TRACE("Socket closed...~n", []);
	Other ->
		% otherwise, wha huh?
		?TRACE("Unknown message: ~p~n", [Other])
    end.

listen_loop(LS, StarterName) ->
	inet:setopts(LS, [{active, once}]),
	receive
		{tcp, Socket, Data} ->
			case binary_to_term(Data) of
				{deliver, Message} ->					
					StarterPid = utils:live_process_named(StarterName),
					Response = handle_received_message(Message, StarterPid),
					?TRACE("Received message~n", [Message, Response, StarterName]),
					listen_loop(Socket, StarterName);
				{server, _Address, _Port} ->
					?TRACE("Received notification that I'll be receiving stuff~n", []),
					listen_loop(Socket, StarterName);
				{remote, _Address, _Port} ->
					?TRACE("Received notification that I'll be receiving stuff from remote~n", []),
					listen_loop(Socket, StarterName);
				{close} ->
					gen_tcp:close(Socket);
				Other ->
					?TRACE("Received ~p message from accept~n", [Other]),
					listen_loop(Socket, StarterName)
			end;
		{tcp_closed,S} ->
			?TRACE("Socket ~w closed [~w]~n",[S,self()]),
            ok;
		{'EXIT', _From, normal} ->
			?TRACE("Received normal EXIT~n", []);
		{'EXIT', _From, Reason} ->
			?TRACE("Received EXIT because ~p~n", [Reason]);
		Other ->
			?TRACE("Unknown message received", [Other]),
			listen_loop(LS, StarterName)
	end.

open_port_for_listening(Port) ->
	case gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, 
					      {active, once}]) of
	{ok, Socket} ->
	    Socket;
	Else ->
	    ?TRACE("Error: can't listen on ~p: ~p~n", [Port, Else]),
		{stop, Else}
    end.

handle_received_message(Message, StarterPid) ->
	StarterPid ! Message.
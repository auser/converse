%%%%%
% This handles the connection creation and connection
%%%%%

-module (talker_connection).

-export([send/2, open_new_connection_to/4, new/3]).

-include("talker.hrl").

new(Address, Port, Socket) -> 
	spawn(fun() -> loop(Socket, Address, Port) end).

open_new_connection_to(Address, Port, MyAddress, MyPort) ->
	Owner = self(),
	LPid = spawn(fun() ->
			case new_connection(Address, Port, MyPort) of
				fail ->
					Owner ! {new_connection_failed};
				Socket ->
					case MyAddress of
						undefined ->
							{ok, {MyIP, MyPort}} = inet:sockname(Socket),
						     Owner ! {new_connection_started, MyIP, MyPort, Socket},
						     loop(Socket, Address, Port);
						_ ->
							Owner ! {new_connection_started, Socket},
							loop(Socket, Address, Port)
					end
			end
		end),
	receive
		{new_connection_failed} -> 
			fail;
		{new_connection_started, LocalIp, LocalPort, LocalSocket} ->
			{local_connection, LocalIp, LocalPort, LocalSocket};
		{new_connection_started, Socket} ->
			{connection, LPid, Socket}
	end.

send(Node, Message) ->
	Pid = Node#node.pid,	
	Socket = Node#node.socket, Address = Node#node.address, Port = Node#node.port,
	Bin = term_to_binary({deliver, Pid, Message}),
	io:format("Sending ~p with ~p~n", [Bin, Socket]),	
	case gen_tcp:send(Socket, Bin) of
		ok ->
			io:format("Sent message ~p~n", [Message]),
			ok;
		{error, closed} ->
			talker_router:unregister_connection(Address, Port),
			gen_tcp:close(Socket);
		{error, Reason} ->
			io:format("Error: ~p:~p ~p~n", [Address, Port, Reason]),
			talker_router:unregister_connection(Address, Port),
			gen_tcp:close(Socket)
	end.

loop(fail, Address, Port) ->
	talker_router:unregister_connection(Address, Port),
	ok;

loop(Socket, Address, Port) ->
	receive
		{send, Message} ->
			case send(#node{address=Address, port=Port, socket=Socket}, Message) of
				ok ->
					loop(Socket, Address, Port);
				_ ->
					ok
			end;
		{tcp_closed, Socket} ->
			% remove the connection if the tcp socket has closed
			talker_router:unregister_connection(Address, Port),
			gen_tcp:close(Socket);
		% if we have received a tcp message
		{tcp, Socket, Data} ->
			case binary_to_term(Data) of
				{deliver, Process, Message} ->
					Process ! Message,
					inet:setopts(Socket, [{active, once}]),
					loop(Socket, Address, Port);
				{user_close} ->
					talker_router:unregister_connection(Address, Port),
					gen_tcp:close(Socket);
				Unknown ->
					io:trace("Unknown message: ~p~n", [Unknown]),
					inet:setopts(Socket, [{active, once}]),
					loop(Socket, Address, Port)					
			end;
		Unknown ->
			io:trace("Unknown message: ~p~n", [Unknown]),
			loop(Socket, Address, Port)
	end.

new_connection(Address, Port, MyPort) ->
    case gen_tcp:connect(Address, Port, ?PACKET_SETUP, ?TIMEOUT) of
        {ok, Socket} ->
	    case inet:sockname(Socket) of
		{ok, {MyAddress, _MyPort}} ->
	            gen_tcp:send(Socket, term_to_binary({server, MyAddress, MyPort})),
		    case inet:peername(Socket) of
			{ok, {RemoteIP, RemotePort}} ->
		            gen_tcp:send(Socket, term_to_binary({youare, RemoteIP, RemotePort})),
		            Socket;
			{error, Reason} ->
			    io:format("reconnect to ~p because socket is ~p~n", [Address, Reason]),
			    gen_tcp:close(Socket),
	    		    new_connection(Address, Port, MyPort)
		    end;
		{error, Reason} ->
		    io:format("reconnect to ~p because socket is ~p~n", [Address, Reason]),
		    gen_tcp:close(Socket),
	            new_connection(Address, Port, MyPort)
	    end;
        {error, Reason} ->
            io:format("couldn't connect to ~p:~p (~p)~n", [Address, Port, Reason]),
		    fail
    end.

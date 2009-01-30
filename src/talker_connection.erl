%%%%%
% This handles the connection creation and connection
%%%%%

-module (talker_connection).

-export([send/3, open_new_connection_to/4, new/3, open_port_for_listening/2]).

-include("talker.hrl").

new(Address, Port, Socket) -> 
	spawn(fun() -> loop(Socket, Address, Port) end).

open_new_connection_to(Address, Port, _NewAddress, MyPort) ->
	Owner = self(),
	LPid = spawn(fun() ->
			case new_connection(Address, Port, MyPort) of
				fail ->
					Owner ! {new_connection_failed};
				Socket ->
					Owner ! {new_connection_started, Socket},
					loop(Socket, Address, Port)
			end
		end),
	receive
		{new_connection_failed} -> fail;
		{new_connection_started, Socket} ->
			{connection, LPid, Socket}
	end.

send({Address, Port, Socket}, Pid, Message) ->
	Bin = term_to_binary({deliver, Pid, Message}),
	case gen_tcp:send(Socket, Bin) of
		ok ->
			io:format("Sent message ~p~n", [Message]),
			ok;
		{error, closed} ->
			talker_router:unregister_connection(Address, Port),
			gen_tcp:close(Socket);
		{error, Reason} ->
			io:format("Error: ~p~p~p~n", [Address, Port, Reason]),
			talker_router:unregister_connection(Address, Port),
			gen_tcp:close(Socket)
	end.

loop(fail, Address, Port) ->
	talker_router:unregister_connection(Address, Port),
	ok;

loop(Socket, Address, Port) ->
	receive
		{send, Pid, Message} ->
			case send({Address, Port, Socket}, Pid, Message) of
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
    case gen_tcp:connect(Address, Port, ?PACKET_SETUP, 15000) of
        {ok, Socket} ->
	    case inet:sockname(Socket) of
		{ok, {MyAddress, _MyPort}} ->
	            gen_tcp:send(Socket, term_to_binary({endpoint, MyAddress, MyPort})),
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
            io_lib:format("couldn't connect to ~p:~p (~p)~n", [Address, Port, Reason]),
	    fail
    end.

open_port_for_listening(Port, Ip) ->
	io:format("In open_port_for_listening with ~p:~p~n", [Port, Ip]),
	case gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, 
					      {active, once}]) of
	{ok, Socket} ->
	    Socket;
	Else ->
	    io:format("Error: can't listen on ~p: ~p~n", [Port, Else]),
		{stop, Else}
    end.

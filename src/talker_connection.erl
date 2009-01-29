%%%%%
% This handles the connection creation and connection
%%%%%

-module (talker_connection).

-export([send/3, open_new_connection/4, new/3]).

-include("talker.hrl").

new(Address, Port, Socket) -> 
	spawn(fun() -> loop(Socket, Address, Port) end).

open_new(Address, Port, undefined, MyPort) ->
	Myself = self(),
	LPid = spawn(fun() ->
		case new_connection(Address, Port, MyPort) of
			fail ->
				Myself ! {new_connection_failed};
			Socket ->
				{ok, {MyIP, _Port}} = inet:sockname(Socket),
				Myself ! {new_connection_started, MyIP, MyPort, Socket},
				loop(Socket, Address, Port)
			end
		end),
	receive
		{new_connection_failed} -> fail;
		{new_connection_started, MyIP, MyPort, Sock} ->
			{local_ip, MyIP, MyPort, LPid, Sock}
	end;

open_new(Address, Port, _NewAddress, MyPort) ->
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
			talker_router:unregister_connection(Address, Port),
			gen_tcp:close(Socket);
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
            log:log2file(comm_connection, io_lib:format("couldn't connect to ~p:~p (~p)~n", [Address, Port, Reason])),
	    fail
    end.


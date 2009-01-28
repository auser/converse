%%%%%
% This handles the connection creation and connection
%%%%%

-module (talker_connection).

-export([send/3, open_new/4, new/3]).

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
		{new_connection_failed} -> fail
		{new_connection_started, MyIP, MyPort, Sock} ->
			{local_ip, MyIP, MyPort, LPid, Sock}
	end;

open_new({Address, Port, Socket}, Pid, Message) ->
	
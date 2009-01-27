-module (talk_to).

-export([send_message/3, open_new/4, new/3]).

new(Add, Port, Socket) ->
	spawn(fun() -> loop(recv_for(Socket, Add, Port)) end).

open_new(Add, Port, undefined, MyPort) ->
    LocalPid = spawn(fun () ->
  			     case new_connection(Add, Port, MyPort) of
				 Socket ->
				     {ok, {MyIP, _MyPort}} = inet:sockname(Socket),
				     self() ! {new_connection_started, MyIP, MyPort, Socket},
				     loop(Socket, Address, Port);
				 error -> 
				     self() ! {new_connection_failed}
			     end
  		     end),
    receive
  	{new_connection_failed} ->
	    error;
  	{new_connection_started, MyIP, MyPort, Sock} ->
  	    {local_address, MyIP, MyPort, LocalPid, Sock}
    end;
open_new(Address, Port, _MyAddress, MyPort) ->
    Owner = self(),
    LocalPid = spawn(fun () ->
			     case new_connection(Address, Port, MyPort) of
				 error ->
				     Owner ! {new_connection_failed};
				 Socket ->
				     Owner ! {new_connection_started, Socket},
				     recv_for(Socket, Address, Port) 
			     end
		     end),
    receive
	{new_connection_failed} ->
	    error;
	{new_connection_started, Socket} ->
	    {connection, LocalPid, Socket}
    end.


send_message({Add, Port, Sock}, Pid, Message) ->
	Bin = term_to_binary({deliver, Pid, Message}),
	case gen_tcp:send(Sock, Bin) of
		ok ->
			ok;
		{error, close} ->
			talk_router:remove_connection(Add, Port),
			gen_tcp:close(Sock);
		{error, Reason} ->
			talk_router:remove_connection(Add, Port),
			gen_tcp:close(Sock)
	end.

recv_for(error, Add, Port) ->
	talk_router:remove_connection(Add, Port),
	ok;
	
recv_for(Socket, Add, Port) ->
	receive
		{send, Pid, Message} ->
			case send({Add, Port, Socket}, Pid, Message) of
				ok ->
					recv_for(Socket, Add, Port);
				_ ->
					ok
			end;
		{tcp_closed, Socket} ->
			talk_router:remove_connection(Add, Port),
			gen_tcp:close(Socket);
		{tcp, Socket, Data} ->
			case binary_to_term(Data) of
				{deliver, Pid, Message} ->
					Pid ! Message,
					inet:setopts(Socket, [{active, once}]),
					recv_for(Socket, Add, Port);
				{user_close} ->
					talk_router:remove_connection(Add, Port),
					gen_tcp:close(Socket);
				{you_are, _A, _P} ->
					inet:setopts(Socket, [{active, once}]),
					recv_for(Socket, Add, Port);
				Other ->
					recv_for(Socket, Add, Port)
			end;
		{you_are, _I, _P} ->
			recv_for(Socket, Add, Port);
		Other ->
			recv_for(Socket, Add, Port)
	end.

new_connection(Add, Port, MyPort) ->
	case gen_tcp:connect(Add, Port, [binary, {packet, 4}, {nodelay, true}, {active, once}], 10000) of
		{ok, Socket} ->
			case inet:sockname(Socket) of
				{ok, {MyAddress, _MPort}} ->
					gen_tcp:send(Socket, term_to_binary({endpoint, MyAddress, MyPort})),
					case inet:peername(Socket) of
						{ok, {RIP, RPort}} ->
							gen_tcp:send(Socket, term_to_binary({you_are, RIP, RPort})),
							Socket;
						{error, Message} ->
							gen_tcp:close(Socket),
							new_connection(Add, Port, MyPort)
					end;
				{error, Reason} ->
					gen_tcp:close(Socket),
					new_connection(Add, Port, MyPort)
			end;
		{error, Reason} ->
			io:format("Could not connect to ~p at ~p because ~p~n", [Add, Port, Reason]),
			error
	end.
-module (talk_listener).
-include("talker.hrl").

-export ([start_link/1, init/2]).

start_link(Port) ->
	io:format("Starting listener ~p~n", [Port]),
	Pid = proc_lib:spawn_link(?MODULE, init, [Port, self()]),	
	io:format("Started listener ~p~n", [Pid]),
	receive
		{started} ->
			{ok, Pid}
	end.
	
init(Port, Super) ->
	ListeningSocket = open_port_for_listening(Port, talker_router:my_ip()),
	{ok, {Ip, _}} = inet:sockname(ListeningSocket),
	talker_router:set_local_address(Ip, Port),
	Super ! {started},
	server(ListeningSocket).

server(LS) ->
    case gen_tcp:accept(LS) of
	{ok, S} ->
	    case talker_router:get_local_address_port() of
		{undefined, LocalPort} ->
		    {ok, {MyIP, _LocalPort}} = inet:sockname(S),
			talker_router:set_local_address(MyIP, LocalPort);
		_ ->
		    ok
	    end,
	    receive
		{tcp, S, Msg} ->
		    {endpoint, Address, Port} = binary_to_term(Msg),
 		    NewAddress = if Address =:= {0,0,0,0} orelse Address =:= {127,0,0,1} ->  
 			    case inet:peername(S) of
 				{ok, {PeerAddress, _Port}} -> 
				    PeerAddress;
 				{error, _Why} ->
				    Address
 			    end;
  			true ->
			    Address
		    end,
		    NewPid = talker_connection:new(NewAddress, Port, S),
			io:format("Connecting using the pid: ~p~n", [NewPid]),
		    gen_tcp:controlling_process(S, NewPid),
		    inet:setopts(S, [{active, once}]),
		    talker_router:register_connection(NewAddress, Port, NewPid, S)
	    end,
	    server(LS);
	Other ->
		io:format("Unknown message: ~p~n", [Other])
    end.

open_port_for_listening(Port, Ip) ->
	io:format("in open_port_for_listening with ~p:~p~n", [Port, Ip]),
	case gen_tcp:listen(Port, [binary, {packet, 4}, {reuseaddr, true}, 
					      {active, once}]) of
	{ok, Socket} ->
	    Socket;
	Else ->
	    io:format("Error: can't listen on ~p: ~p~n", [Port, Else]),
		{stop, Else}
    end.
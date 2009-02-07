-module (converse).

-behaviour (application).
-revision ("Revision 0.1").
-compile (export_all).

-export ([start/2, stop/1]).

start(Fun) ->
	converse_app:start(normal, [Fun]).

start(_Type, Args) ->
	converse_supervisor:start_link(Args).

stop(_State) ->
	ok.
	
open_and_send({Address, Port}, Data) ->
	{ok, Socket} = gen_tcp:connect(Address, Port, [{packet, 2}]),
	send_to_open(Socket, Data),
	{ok, Socket}.
	
send_to_open(Socket, Data) ->
	gen_tcp:send(Socket, converse_packet:encode(Data)),
	{ok, Socket}.
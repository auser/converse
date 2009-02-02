-module (converse).
-include ("converse.hrl").
-compile (export_all).

-import(io).

start_link(Port, Starter) ->
    Pid = converse_supervisor:start_link(Port, Starter),
	?TRACE("Started supervisor ~p~n", [Pid]).
	
start_link_for_testing( Port ) ->	
	?TRACE("Started supervisor...~n", []),
	NewPid = spawn(fun() -> do_test_receive() end),
	?TRACE("Started watch pid ~p...~n", [NewPid]),
	erlang:register(test_receiver, NewPid),
	converse_supervisor:start_in_shell_for_testing(Port, test_receiver ).
	
send({Address, Port}, Message) ->
	converse_connector:send({Address, Port}, Message).

do_test_receive() ->
	spawn(fun() ->		
		receive
			{done} ->
				ok;
			Anything ->
				?TRACE("Test receive: ~p~n", [Anything]),
				do_test_receive()
		end
	end).
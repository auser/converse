-module (converse).
-include ("converse.hrl").
-compile (export_all).

start(Fun, Config) ->
	NewConfig = [?DEFAULT_CONFIG|Config],
	case converse_supervisor:start_link(Fun, NewConfig) of
		{error, Error} ->
			?TRACE("Received shutdown error...", [Error]);
		{ok, Pid} ->
			?TRACE("Started converse", [Pid]),
			Pid
	end.	
	
start(Fun) ->
	Config = [],
	start(Fun, Config).

stop() ->
	converse_tcp:stop().

send(At, Data) ->
	converse_tcp:send(At, Data).
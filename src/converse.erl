-module (converse).
-include ("converse.hrl").
-compile (export_all).

start(Fun, Config) ->
	?TRACE("In start/2", []),
	[NewConfig] = case length(Config) > 0 of
		true -> [ config:update(Key, Value, ?DEFAULT_CONFIG) || {Key, Value} <- Config ];
		false -> [?DEFAULT_CONFIG]
	end,
	case converse_supervisor:start_link(Fun, NewConfig) of
		{error, Error} ->
			?TRACE("Received shutdown error...", [Error]);
		{ok, Pid} ->
			?TRACE("Started converse", [Pid]),
			Pid
	end.	
	
start(Args) ->
	?TRACE("In start/1", []),
	[M|Rest] = Args, [F|OtherArgs] = Rest,
	A = utils:parse_args(OtherArgs),
	?TRACE("start()", [M,F,A]),
	start([M,F],A).

stop() ->
	converse_tcp:stop().

send(At, Data) ->
	converse_tcp:send(At, Data).
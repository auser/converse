-module (test_app).
-compile (export_all).

start() ->
	converse:start(normal, [{receive_function, [?MODULE, receive_function]}]).

receive_function(From) ->
	receive
		{data, Socket, Data} ->
			case Data of
				{data, Message} ->
					io:format("Received ok ~p from ~p~n", [Message, Socket]),
					receive_function(From);
				{who_are_you} ->
					io:format("Received who are you from ~p~n", [Socket]),
					receive_function(From)
			end;
		Anything ->
			io:format("Received ~p~n", [Anything]),
			Reply = {ok, "received"},
			From ! {reply, Reply},
			receive_function(From)
	end.

hear(From) ->
	receive
		Anything ->
			io:format("Heard ~p~n", [From]),
			hear(From)
	end.
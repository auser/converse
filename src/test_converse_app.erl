-module (test_converse_app).

-export ([receive_function/0, start/0]).
-export ([go/0]).

start() ->
	converse:start(fun() -> ?MODULE:receive_function() end).

go() ->
	converse:send({"0.0.0.0", 7899}, {hi}).

receive_function() ->
	io:format("-------- Started receive_function in test_converse_app~n"),
	receive
		Anything ->
			io:format("-------- Test listen received ~p~n", [Anything]),
			receive_function()
	end.
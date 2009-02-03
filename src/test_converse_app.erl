-module (test_converse_app).

-export ([receive_function/0, start/0]).
-export ([go/0]).

start() ->
	converse:start(fun() -> ?MODULE:receive_function() end).

go() ->
	start(),
	converse:send({"0.0.0.0", 7899}, {frank, "Hello world"}).

receive_function() ->
	io:format("-------- Started receive_function in test_converse_app~n"),
	receive
		{frank, Says} ->
			io:format("Frank says ~p~n", [Says]);
		Anything ->
			io:format("-------- Test listen received ~p~n", [Anything]),
			receive_function()
	end.
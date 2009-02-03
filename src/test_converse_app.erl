-module (test_converse_app).

-export ([receive_function/0, start/0]).
-export ([go/0]).

start() ->
	converse:start(fun() -> ?MODULE:receive_function() end).

go() ->
	start(),
	converse:send({"0.0.0.0", 7899}, {frank, "Hello world"}),
	converse:send({"0.0.0.0", 7899}, {what, "I don't know"}).

receive_function() ->
	receive
		{frank, Says} ->
			io:format("Frank says ~p~n", [Says]),
			receive_function();
		Anything ->
			io:format("Received something else ~p~n", [Anything]),
			receive_function()
	end.
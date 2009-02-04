-module (test_app).

-export ([receive_function/0, start/0, stop/0]).
-export ([go/0]).

start() ->
	Pid = converse:start(fun() -> ?MODULE:receive_function() end),
	Pid.

stop() ->
	converse:stop().

go() ->
	converse:send({"0.0.0.0", 7899}, {frank, "Hello bob"}),
	converse:send({"0.0.0.0", 7899}, {bob, "Thanks Frank, hello to you too!"}),
	converse:send({"0.0.0.0", 7899}, {frank, "You are welcome bob"}).

receive_function() ->
	receive
		{frank, Says} ->
			io:format("Frank says ~p~n", [Says]),
			receive_function();
		{bob, Says} ->
			io:format("Bob says ~p~n", [Says]),
			receive_function();
		Anything ->
			io:format("Received something else ~p~n", [Anything]),
			receive_function()
	end.
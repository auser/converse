-module (test_app).
-compile (export_all).

start() ->
	tcp_server_app:start(normal, [?MODULE, receive_function]).

receive_function() ->
	receive
		Anything ->
			io:format("Received ~p~n", [Anything]),
			receive_function()
	end.
-module (converse_test_app).
-compile(export_all).

layers_receive() ->
	receive
		Anything ->
			layers_log:info("Received ~p~n", [Anything]),
			layers_receive()
	end.

start(_Type, Config) ->
	layers:start([converse, ?MODULE], [{port, 1234}]),
	ok.
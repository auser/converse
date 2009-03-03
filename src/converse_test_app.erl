-module (converse_test_app).
-compile (export_all).
-behaviour (application).

-define (PORT, 22002).

start_layers() ->	
	layers:init(),
	layers:add(converse, [{port, ?PORT}]),
	layers:add(converse_test_app, []),
	layers:start().

start(_Type, Config) ->
	io:format("Starting converse_test_app with ~p~n", [Config]).
	
init([Config]) ->
	io:format("Starting converse_test_app (init) with ~p~n", [Config]).
	
test() ->
	converse:send_message(9009, {ping}).

layers_receive(Msg) ->
  case Msg of
    {data, Socket, Data} ->
      io:format("Unencrypted in ~p data: ~p~n", [?MODULE, Data]),
      Reply = converse:reply(Socket, {data, Data}),
      Reply;
    Anything ->
      io:format("layers_receive Received: ~p (in ~p)~n", [Anything, ?MODULE])
  end.
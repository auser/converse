converse:start(normal, []).
converse:send_message({data, "hey"}).

{ok, Sock} = gen_tcp:connect({0,0,0,0}, 22002, [binary]).
gen_tcp:send(Sock, converse_socket:encode({data, "hey"})).

receive
  {reply, Reply} -> io:format("Received ~p~n", [Reply])
  after 1000 -> ok
end.

{ok, Sock} = gen_tcp:connect({10,45,10,228}, 22002, [binary]).

converse:send({0,0,0,0}, {"hi"}).

layers:start([converse, layers_test_app], []).
converse:send({0,0,0,0}, {"hi"}).
converse:send({0,0,0,1}, {"hi"}).
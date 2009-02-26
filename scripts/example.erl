converse:start(normal, []).
converse:send_message("0.0.0.0", "hey").
converse:send_message("0.0.0.0", 12345).
converse:cast_message("0.0.0.0", "hey").

f().
{ok, Sock} = gen_tcp:connect({0,0,0,0}, 22002, [binary]).
gen_tcp:send(Sock, term_to_binary({data, "hey"})).
receive {reply, S, M} -> M after 1000 -> no_response end.

receive
  {reply, Reply} -> io:format("Received ~p~n", [Reply])
  after 1000 -> ok
end.

{ok, Sock} = gen_tcp:connect({10,45,10,228}, 22002, [binary]).

converse:send({0,0,0,0}, {"hi"}).

layers:start([converse, layers_test_app], []).
converse:send({0,0,0,0}, {"hi"}).
converse:send({0,0,0,1}, {"hi"}).
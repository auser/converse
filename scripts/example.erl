converse:start(normal, []).
{ok, Sock} = gen_tcp:connect({0,0,0,0}, 22002, [binary]).
gen_tcp:send(Sock, converse_packet:encode({data, "hey"})).

converse:send({0,0,0,0}, {"hi"}).

layers:start([converse, layers_test_app], []).
converse:send({0,0,0,0}, {"hi"}).
converse:send({0,0,0,1}, {"hi"}).
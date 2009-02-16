converse:start(normal, [{port, 1234}, {successor, [whisper]}]).
{ok, Sock} = converse:open_and_send({{0,0,0,0}, 1234}, {data, "hi"}).
converse:send_to_open(Sock, {data, "yo"}).

% converse:open_and_send({{0,0,0,0}, 7899}, {data, "hi"}).

% P = utils:get_child_pid(converse, tcp_server).
% P ! {change_receiver, [test_app, hear]}.
% test_app:start().
layser:init().
layers:add(converse, [{port, 1234}]).
layers:add(whisper, []).
layers:add(email, [{to_email, "alerner@att.com"}]).
layers:add(test_app, []).
layers:start().
==
layers:start([converse, whisper, test_app], [{port, 1234}]).
converse:echo({{0,0,0,0}, 1234}).
converse:open_and_send({0,0,0,0}, {data, whisper:encrypt("hi")}).

converse:open_and_send({{10,45,10,62}, 1234}, {data, whisper:encrypt("hi")}).

{ok, Sock} = converse:open_and_send({{0,0,0,0}, 1234}, {data, whisper:encrypt("hi")}).
converse:send_to_open(Sock, {data, "yo"}).

converse:start(normal, [{layers_receive, [test_app,layers_receive]}, {port, 1235}]).
{ok, Sock} = converse:open_and_send({{0,0,0,0}, 1235}, {data, "hi"}).
converse:send_to_open(Sock, {data, "yo"}).

{ok, Socket} = gen_udp:open(0, [binary]).
ok = gen_udp:send(Socket, {0,0,0,0}, 1235, <<"hi">>).

f(S),{ok, S} = gen_tcp:connect({0,0,0,0}, 8021, [{packet, 2}]).
gen_tcp:send(S, <<"hello">>).

converse:start(normal, [{successor, [converse]}, {port, 1235}]).
converse:open_and_send({0,0,0,0}, {data, "hi"}).

layers:start([converse, whisper, test_app], [{port, 22001}]).
converse:open_and_send({97,94,97,10}, {data, "hi"}).
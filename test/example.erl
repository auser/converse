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
{ok, Sock} = converse:open_and_send({{0,0,0,0}, 1234}, {data, whisper:encrypt("hi")}).
converse:send_to_open(Sock, {data, "yo"}).

converse:start(normal, [{receive_function, [test_app,receive_function]}, {port, 1235}]).
{ok, Sock} = converse:open_and_send({{0,0,0,0}, 1235}, {data, "hi"}).
converse:send_to_open(Sock, {data, "yo"}).

{ok, Socket} = gen_udp:open(0, [binary]).
ok = gen_udp:send(Socket, {0,0,0,0}, 1235, <<"hi">>).
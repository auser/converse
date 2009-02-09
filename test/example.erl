converse:start(normal, [{receive_function, [test_app,receive_function]}, {port, 1235}]).
converse_listener:start_link(tcp_app_fsm, [{receive_function, [test_app,receive_function]}, {port, 1235}]).
% converse:open_and_send({{0,0,0,0}, 7899}, {data, "hi"}).

% P = utils:get_child_pid(converse, tcp_server).
% P ! {change_receiver, [test_app, hear]}.
% test_app:start().
layers:start([converse, test_app], [{port, 1235}]).

{ok, Sock} = converse:open_and_send({{0,0,0,0}, 1235}, {data, "hi"}).
converse:send_to_open(Sock, {data, "yo"}).

converse:start(normal, [{receive_function, [test_app,receive_function]}, {port, 1235}]).
{ok, Sock} = converse:open_and_send({{0,0,0,0}, 1235}, {data, "hi"}).
converse:send_to_open(Sock, {data, "yo"}).

{ok, Socket} = gen_udp:open(0, [binary]).
ok = gen_udp:send(Socket, {0,0,0,0}, 1235, <<"hi">>).
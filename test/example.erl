converse:start(fun() -> test_app:receive_function() end, [{port, 1235}]).

converse_tcp:start_link(fun() -> test_converse_app:receive_function() end, [{port, 1234}]).

converse_supervisor:start_link(fun() -> test_converse_app:receive_function() end, [{port, 1234}]).

converse_supervisor:start_link(fun() -> test_converse_app:receive_function() end, [{port, 1234}]).
converse_supervisor:start_link(fun() -> test_converse_app:receive_function() end, [{port, 1235}]).
converse:send({{0,0,0,0}, 1234}, {frank, "bee bop"}).

converse:send({{0,0,0,0}, 1235}, {frank, "bee bop"}).

ReceiverFunction = fun() -> test_app:receive_function() end.
{ok, Pid} = gen_server:start_link(converse_tcp, [[{port, 1234}], ReceiverFunction], []).
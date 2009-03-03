converse:start(normal, [{port, 9009}]).
converse:send_message(9009, {ping}).
converse:send_message("0.0.0.0", 9009, {ping}).
converse:send_message(9010, {ping}).

converse:start(normal, [{port, 9010}]).

converse_udp:send({ping}, "localhost", 9009).

converse_udp:send("hi", "localhost", 9009).
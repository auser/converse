-module (converse_socket).
-compile (export_all).

% Write methods
write_binary(Socket, Binary) ->
  gen_tcp:send(Socket, Binary).

% Read methods
read_socket(_Socket) ->
	ok.
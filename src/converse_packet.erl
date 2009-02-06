-module (converse_packet).

-export ([encode/1, decode/1]).

encode(Binary) ->
	term_to_binary(Binary).

decode(Packet) ->
	binary_to_term(Packet).
	
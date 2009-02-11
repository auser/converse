-module (converse_packet).

-export ([encode/1, decode/1]).

encode(Binary) ->
	term_to_binary(Binary).

decode(Packets) when is_list(Packets) -> [decode(P) || P <- Packets];
decode(Packet) when is_binary(Packet) -> binary_to_term(Packet);
decode(Packet) -> Packet.
-module (converse_socket).
-include ("converse.hrl").
-compile (export_all).

encode({data, D}) -> erlang:list_to_binary(D);
encode(<<D>>) -> D;
encode(Integer) when is_integer(Integer) -> encode(integer_to_list(Integer));
encode(Plain) when is_list(Plain) -> erlang:term_to_binary(Plain).

decode(Packets) when is_list(Packets) -> [decode(P) || P <- Packets];
decode(Packet) when is_binary(Packet) -> binary_to_term(Packet);
decode(Packet) -> Packet.
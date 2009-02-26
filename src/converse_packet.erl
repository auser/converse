-module (converse_packet).
-include ("converse.hrl").
-compile (export_all).

encode({data, D}) -> erlang:list_to_binary(D);
encode(Integer) when is_integer(Integer) -> encode(integer_to_list(Integer));
encode(Binary) when is_binary(Binary) -> Binary;
encode(Plain) when is_list(Plain) -> erlang:term_to_binary(Plain);
encode(Tuple) when is_tuple(Tuple) -> erlang:term_to_binary(Tuple);
encode(B) -> B.

decode(Packets) when is_list(Packets) -> [decode(P) || P <- Packets];
decode(Packet) when is_binary(Packet) -> binary_to_term(Packet);
decode(Packet) -> Packet.
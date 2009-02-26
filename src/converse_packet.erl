-module (converse_packet).
-include ("converse.hrl").
-compile (export_all).

encode(Integer) when is_integer(Integer) -> encode(integer_to_list(Integer));
encode(Binary) when is_binary(Binary) -> Binary;
encode(Plain) when is_list(Plain) -> erlang:term_to_binary(Plain);
encode(B) -> B.

decode(Packets) when is_list(Packets) -> [decode(P) || P <- Packets];
decode(Bin) when is_binary(Bin) -> 
  io:format("Packet received: ~p~n", [Bin]),
  erlang:binary_to_list(Bin);
decode(<<Packet>>) -> Packet.

pack(Tuple) -> erlang:term_to_binary(Tuple).
unpack(Binary) -> erlang:binary_to_term(Binary).
-module (converse_socket).
-include ("converse.hrl").
-compile (export_all).

encode({data, D}) ->
  erlang:list_to_binary(D);
encode(<<D>>) -> D;
encode(Plain) when is_list(Plain) -> erlang:term_to_binary(Plain).

decode(Packets) when is_list(Packets) -> [decode(P) || P <- Packets];
decode(<<Pack>>) ->
  Pack;
decode(Packet) -> Packet.
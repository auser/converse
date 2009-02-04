-module (converse_packet_test).

-include_lib("../include/eunit/include/eunit.hrl").

packet_encode_test_() ->
  [
    ?_assert( term_to_binary("hi") == converse_packet:encode("hi") )
  ].

packet_decode_test_() ->
  [
    ?_assert( "hi" == converse_packet:decode( term_to_binary("hi") ) )
  ].

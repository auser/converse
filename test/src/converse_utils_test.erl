-module (converse_utils_test).

-include_lib("../include/eunit/include/eunit.hrl").

safe_integer_test_() ->
  [
		% ?_assert( 1000 == utils:safe_integer( "1000" ) ),
		?_assert( 1000 == converse_utils:safe_integer( '1000' ) ),
		?_assert( 1000 == converse_utils:safe_integer( ['1000'] ) ),
    ?_assert( 1000 == converse_utils:safe_integer( 1000 ) )
  ].

parse_args_test_() ->
  [
    ?_assert( [{bob, "10"}, {billy, "20"}] == converse_utils:parse_args([bob, "10", billy, "20"]) )
  ].

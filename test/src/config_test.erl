-module (config_test).
-include_lib("../include/eunit/include/eunit.hrl").

parse_test_() ->
	Config = [{pig, 1}, {cow, "betsy"}],
	[
		?_assert( 1 == config:parse(pig, Config) ),
		?_assert( "betsy" == config:parse(cow, Config) )
	].

update_test_() ->
	Config = [{pig, "bob"}, {cow, "betsy"}],
	[
		?_assert( [{cow, "betsy"},{pig, 10}] == config:update(pig, 10, Config) ),
		?_assert( [{pig, "bob"}, {cow, "betsy"}, {dave, "dave"}] == config:update(dave, "dave", Config) )
	].
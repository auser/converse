-module (converse).
-include ("converse.hrl").
-compile (export_all).

start(Fun) ->
	Config = ?DEFAULT_CONFIG,
	converse_supervisor:start_link(Config, Fun).

send({Address, Port}, Data) ->
	converse_tcp:send({Address, Port}, Data).

set_receive_function(Fun) ->
	converse_tcp:set_receive_function(Fun).
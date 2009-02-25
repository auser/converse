-module (converse).
-include ("converse.hrl").

-export ([start/2]).
-export ([send_with_reply/2]).

start(Type, StartArgs) -> converse_app:start(Type, StartArgs).
	
send_with_reply(Addr, Msg) ->
	converse_listener:send_with_reply(Addr, Msg).
-module (converse).
-include ("converse.hrl").

-export([send/2,send/3]).
-export ([start/2]).

start(Type, StartArgs) -> converse_app:start(Type, StartArgs).

% send(Addr, Msg) -> send(Addr, Msg, ?DEFAULT_TIMEOUT).
send(Addr, Msg, Timeout) -> converse_tcp:send(Addr, Msg, Timeout).

send(Socket, Msg) ->
	gen_tcp:send(Socket, converse_packet:encode(Msg)).
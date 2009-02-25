-module (converse).
-include ("converse.hrl").

-export([send/2,send/3]).
-export ([start/2]).

start(Type, StartArgs) -> 
  converse_app:start(Type, StartArgs).

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

start(normal, [Config]) ->
  [Port] = 
-include_lib("kernel/include/inet.hrl").

-define (NAMESPACE, ?MODULE).
-define (TIMEOUT, 20000).
-define (DEFAULT_PORT, 5002).
-define (TIMES_TO_RETRY, 3).

-define (PACKET_SETUP, [binary, {packet, 4}, {nodelay, true}, {active, once}, {send_timeout, 60000}]).

-define (debug, true).

-ifdef(debug).

-define (TRACE(X, M), io:format("TRACE ~p:~p ~p ~p~n" ,[?MODULE, ?LINE, X, M])).

-endif.
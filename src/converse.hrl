-include_lib("kernel/include/inet.hrl").
-include ("config.hrl").

-define (debug, true).

-define (MAX_CONN, 200).
-define (RETRY_TIMES, 3).
-define (TIMEOUT, 20000).
-define (DEFAULT_PORT, 7899).

-define (DEFAULT_CONFIG, #config{
					max_connections = ?MAX_CONN,
					connection_retries = ?RETRY_TIMES,
					timeout = ?TIMEOUT,
					port = ?DEFAULT_PORT
				}).

-define (PACKET_SETUP, [binary, inet6, {packet, raw}, {reuseaddr, true}, {keepalive, true}, {active, false}]).

-ifdef(debug).
-define (TRACE(X, M), io:format("TRACE ~p:~p ~p ~p~n" ,[?MODULE, ?LINE, X, M])).
-endif.

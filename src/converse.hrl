-include_lib("kernel/include/inet.hrl").

-define (debug, true).

-define (MAX_CONN, 200).
-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).

-define (TIMES_TO_RETRY, 3).
-define (TIMEOUT, 100000).
-define (DEFAULT_PORT, 7899).

-define (DEFAULT_CONFIG, [
					{receiver, undefined},
					{max_connections, ?MAX_CONN},
					{connection_retries, ?TIMES_TO_RETRY},
					{timeout,?TIMEOUT},
					{port, ?DEFAULT_PORT}
				]).

-ifdef(debug).
-define (TRACE(X, M), io:format("TRACE ~p:~p ~p ~p~n" ,[?MODULE, ?LINE, X, M])).
-endif.

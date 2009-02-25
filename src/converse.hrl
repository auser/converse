-define (debug, true).

-define (PORT, 22002).
-define (DEFAULT_TIMEOUT, 5000).
-define (HEARTBEAT_TIMEOUT, 8000).
-define (HEARTBEAT_PERIOD, 15000).
-define (MAX_FAIL_ATTEMPTS, 3).
-define (RETRY_TIME, 6000).
-define (SLOWDOWN_INTERVAL, 500).

-define(LOG_MESSAGE(TAG, SIZE), converse_logger:log(TAG, SIZE)).

-define (DEFAULT_SOCKET_OPTS, [binary,{packet,raw},{active,false}, {reuseaddr,true},{keepalive,true},{backlog,30}]).

-define (DEFAULT_CONFIG, [
					{port, ?PORT},
					{timeout, ?DEFAULT_TIMEOUT},
					{heartbeat_timeout, ?HEARTBEAT_TIMEOUT},
					{close_after_fails, ?MAX_FAIL_ATTEMPTS},
					{retry_time, ?RETRY_TIME},
					{hostname, "localhost"},
					{secret, "secretsdontmakefriends"},
					{queue, false},
					{sock_opts, ?DEFAULT_SOCKET_OPTS}
				]).
				
-ifdef(debug).
-define (TRACE(X, M), io:format("TRACE ~p:~p ~p ~p~n" ,[?MODULE, ?LINE, X, M])).
-endif.

-define (MAXIMUM_RESTARTS, 10).
-define (MAX_DELAY_TIME, 60).
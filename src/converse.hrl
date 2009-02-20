-define (PORT, 22002).
-define (DEFAULT_TIMEOUT, 5000).
-define (HEARTBEAT_TIMEOUT, 8000).
-define (HEARTBEAT_PERIOD, 15000).
-define (MAX_FAIL_ATTEMPTS, 3).
-define (RETRY_TIME, 6000).
-define (SLOWDOWN_INTERVAL, 500).

-record(converse_message_queue, {index, message}).

-define (DEFAULT_SOCKET_OPTS, [binary,{packet,raw},{active, true}, {reuseaddr,true}]).

-define (DEFAULT_CONFIG, [
					{port, ?PORT},
					{timeout, ?DEFAULT_TIMEOUT},
					{heartbeat_timeout, ?HEARTBEAT_TIMEOUT},
					{close_after_fails, ?MAX_FAIL_ATTEMPTS},
					{retry_time, ?RETRY_TIME},
					{hostname, "localhost"},
					{secret, "secretsdontmakefriends"},
					{queue, false}
				]).
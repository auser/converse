-include_lib("kernel/include/inet.hrl").

-define (NAMESPACE, ?MODULE).
-define (TIMEOUT, 20000).
-define (DEFAULT_PORT, 5001).
-define (DB, ets).

% Records
-record (node, {address, port, pid, socket, tuple}).
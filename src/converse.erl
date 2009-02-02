-module (converse).

-compile (export_all).

-import(io).

start_link() ->
    Pid = converse_supervisor:start_link(),
	io:format("Started supervisor ~p~n", [Pid]).

send({Address, Port}, Message) ->
	converse_router:send({Address, Port}, Message).

add(Address, Port, Info) ->
	converse_router:register_connection(Address, Port, Info).

remove(Address, Port) ->
	converse_router:unregister_connection(Address, Port).

find(Address, Port) ->
	converse_router:find_node(Address, Port).

clear() ->
	converse_db:delete_all(node).

remote() ->
	converse_router:all_connections().

all() ->
	all(undefined).
	
all(Type) ->
	converse_router:all(Type).
	
all_pids() ->
	converse_router:all_pids().

self() ->
	converse_router:local_pid().
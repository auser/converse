-module (talk).

-compile (export_all).

-import(io).

start_link() ->
    Pid = talker_supervisor:start_link(),
	io:format("Started supervisor ~p~n", [Pid]).

send({Address, Port}, Message) ->
	talker_router:send({Address, Port}, Message).

add(Address, Port, Info) ->
	talker_router:register_connection(Address, Port, Info).

remove(Address, Port) ->
	talker_router:unregister_connection(Address, Port).

find(Address, Port) ->
	talker_router:find_node(Address, Port).

clear() ->
	talker_db:delete_all(node).

remote() ->
	talker_router:all_connections().

all() ->
	talker_router:all().
	
all_pids() ->
	talker_router:all_pids().

self() ->
	talker_router:local_pid().
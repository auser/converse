-module (talker).

-compile (export_all).

-import(io).

start_link() ->
    Pid = talker_supervisor:start(),
	io:format("Started supervisor ~p~n", [Pid]).

send({Address, Port}, Message) ->
	talker_router:send({Address, Port}, Message).

add(Address, Port, Info) ->
	talker_router:register_connection(Address, Port, Info).

remove(Address, Port) ->
	talker_router:unregister_connection(Address, Port).

clear() ->
	talker_db:delete_all(node).

this() ->
    here(self()).

here(Pid) ->
    {LocalIP, LocalPort} = talker_router:get_local_address_port(),
    {LocalIP, LocalPort, Pid}.
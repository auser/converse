-module (talker).

-export([start_link/0, send/2, this/0, here/1]).

-import(io).

start_link() ->
    Pid = talker_supervisor:start(),
	io:format("Started supervisor ~p~n", [Pid]).

send({Address, Port}, Message) ->
	talker_router:send({Address, Port}, Message).

this() ->
    here(self()).

here(Pid) ->
    {LocalIP, LocalPort} = talker_router:get_local_address_port(),
    {LocalIP, LocalPort, Pid}.
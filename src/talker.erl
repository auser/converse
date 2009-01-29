-module (talker).

-export([start_link/0, send/2, this/0, here/1]).

-import(io).

start_link() ->
	io:format("Starting sup~n"),
    Pid = talker_supervisor:start_link(),
	io:format("Started supervisor ~p~n", [Pid]).

send({{_IP1, _IP2, _IP3, _IP4} = Address, Port} = _Target, Message) ->
	talker_router:send({Address, Port}, Message).
		
this() ->
    here(self()).

here(Pid) ->
    {LocalIP, LocalPort} = talker_router:get_local_address_port(),
    {LocalIP, LocalPort, Pid}.
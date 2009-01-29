-module (talker).

-export([start_link/0, send/2, this/0, here/1]).

-import(io).

start_link() ->
	io:format("Starting sup~n"),
    talker_supervisor:start_link().

send({{_IP1, _IP2, _IP3, _IP4} = _IP, _Port, _Pid} = Target, Message) ->
    {MyIP,MyPort} = talker_router:get_local_address_port(),
    %io:format("send: ~p:~p -> ~p:~p(~p) : ~p\n", [MyIP, MyPort, _IP, _Port, _Pid, Message]),
    IsLocal = (MyIP == _IP) and (MyPort == _Port),
    if
 	IsLocal ->
 	    _Pid ! Message;
 	true ->
	    talker_router:send(Target, Message)
    end;

this() ->
    here(self()).

here(Pid) ->
    {LocalIP, LocalPort} = talker_router:get_local_address_port(),
    {LocalIP, LocalPort, Pid}.
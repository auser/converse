-module (talker).

-export([start_link/0, send/2, this/0, here/1]).

-import(io).
-import(util).

start_link() ->
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

send(Target, Message) ->
    io:format("wrong call to cs_send:send: ~w ! ~w~n", [Target, Message]),
    ok.

%% @doc returns process descriptor for the calling process
-spec(this/0 :: () -> process_id()).
this() ->
    here(self()).

-spec(here/1 :: (pid()) -> process_id()).
here(Pid) ->
    {LocalIP, LocalPort} = talker_router:get_local_address_port(),
    {LocalIP, LocalPort, Pid}.

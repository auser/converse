-module (talk_to).

-export([start_link/0, send/2, this/0, here/1]).


start_link() ->
    talk_supervisor:start_link().

% send(process_id(), term()) -> ok
% {ok, Pid} = talk_to:start_link().
% talk_router:set_my_address({0,0,0,0}, 5001)
% talk_to:send({{0,0,0,0}, 5001, Pid}, {connect}).
send({{_IP1, _IP2, _IP3, _IP4} = _IP, _Port, _Pid} = Target, Message) ->
    {MyIP,MyPort} = talk_router:get_my_address(),
    %io:format("send: ~p:~p -> ~p:~p(~p) : ~p\n", [MyIP, MyPort, _IP, _Port, _Pid, Message]),
    IsLocal = (MyIP == _IP) and (MyPort == _Port),
    if
 	IsLocal ->
 	    _Pid ! Message;
 	true ->
	    talk_router:send_message(Target, Message)
    end;

send(Target, Message) ->
    io:format("error: ~w ! ~w~n", [Target, Message]),
    ok.

this() -> here(self()).

here(Pid) ->
    {LocalIP, LocalPort} = talk_router:get_my_address(),
    {LocalIP, LocalPort, Pid}.

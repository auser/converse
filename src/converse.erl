-module (converse).

-compile (export_all).

-import(io).

start_link(Port, Starter) ->
    Pid = converse_supervisor:start_link(Port, Starter),
	io:format("Started supervisor ~p~n", [Pid]).

send({Address, Port}, Message) ->
	converse_connector:send({Address, Port}, Message).

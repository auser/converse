-module (converse).

-behaviour (application).
-revision ("Revision 0.1").

-export ([start/2, stop/1]).

start(_Type, Args) ->
	io:fwrite("Converse~n"),
	converse_supervisor:start_link(Args).

stop(_State) ->
	ok.
-module (converse_app).
-include("converse.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_Application, []) ->
	Config = ?DEFAULT_CONFIG,
	converse_supervisor:start_link(Config).

stop(Application) ->
	exit(Application, shutdown),
	ok.
-module (converse_supervisor).
-include("converse.hrl").

-behaviour(supervisor).

%% API
-export([start_link/0,start_link/1,start_link/2,start_children/3
        ]).

%% Supervisor callbacks
-export([init/1]).
-define(RESTARTTIME, timer:minutes(10) div timer:seconds(1)).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link(Args::any()) -> {ok,Pid} | ignore | {error,Error}
%% @doc: Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
	io:format("Error. You must specify a receive function~n"),
	exit({error, no_receive_function}).

start_link(Fun) ->
	start_link(Fun, ?DEFAULT_CONFIG).

start_link(Fun, Config) ->
	start_children(Fun, Config, 1).

start_children(Fun, Config, Count) ->
   Children = [ child(Fun, Config, N) || N <- lists:seq(1,Count) ],
   supervisor:start_link(?MODULE, [Children]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init
%% @spec (Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                 ignore                          |
%%                 {error, Reason}
%% @doc Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%% @end
%%--------------------------------------------------------------------
init([Children]) ->
    {ok,{{one_for_one,1,?RESTARTTIME}, Children}}.

child(Fun, Config, Number) ->
   {child_id(Config, Number),
    {converse_tcp, start_link, [Fun, Config]},
    transient, timer:seconds(2), worker,
    [converse_tcp]}.

child_id(Config, Number) ->
	Port = config:parse(port, Config),	
  integer_to_list(utils:safe_integer(Port)) ++ "_" ++ integer_to_list(Number).
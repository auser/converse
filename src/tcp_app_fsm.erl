-module (tcp_app_fsm).
-include ("converse.hrl").
-behaviour(gen_fsm).

-export([start_link/1, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export ([layers_receive/0]).

%% FSM States
-export([
    socket/2,
    data/2
]).

-record(state, {
                socket,    			% client socket
                addr,       		% client address
								successor_mfa		% server accept function
               }).


%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @spec (Socket) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
start_link(Config) ->
   gen_fsm:start_link(?MODULE, [Config], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

% ONLY FOR TESTING PURPOSES
layers_receive() ->
	receive
		Anything -> 
			layers_log:info("~p received message ~p~n", [self(), converse_packet:decode(Anything)]),
			layers_receive()
	end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([Config]) ->
	process_flag(trap_exit, true),
	Fun = config:parse(successor, Config), 
	Successor = case Fun of
		nil -> [?MODULE, layers_receive, []];
		Fun -> Fun
	end,
	{ok, socket, #state{successor_mfa=Successor}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
socket({socket_ready, Socket}, State) when is_port(Socket) ->
	% Now we own the socket
	inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
	{ok, {IP, _Port}} = inet:peername(Socket),
	{next_state, data, State#state{socket=Socket, addr=IP}};

socket(Other, State) ->
	error_logger:error_msg("State: socket. Unexpected message: ~p\n", [Other]),
	%% Allow to receive async messages
	{next_state, socket, State}.

%% Notification event coming from client
data({data, Data}, #state{addr=Ip, socket=S, successor_mfa=Fun} = State) ->
	DataToSend = converse_packet:decode(Data),
	layers:pass(Fun, Data),	
	{next_state, data, State};

data(timeout, State) ->
	error_logger:error_msg("~p Connection timeout - closing.\n", [self()]),
	{stop, normal, State};

data(Data, State) ->
	io:format("~p Ignoring data: ~p\n", [self(), Data]),
	{next_state, data, State, ?TIMEOUT}.

%%-------------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
	{stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
handle_info({tcp, Socket, Bin}, StateName, #state{socket=Socket} = State) ->
	% Flow control: enable forwarding of next TCP message
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({data, Bin}, State);

handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info(Info, StateName, State) ->
	?TRACE("Received info", Info),
	{next_state, StateName, State}.

%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _StateName, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.

%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

get_function(Fun) ->
	case Fun of
		nil -> nil;
		FoundRecFun -> 
			RFun = case FoundRecFun of
				undefined -> self();
				F -> F
			end
	end.
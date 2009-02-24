-module (converse_tcp).
-include ("converse.hrl").
-include_lib("kernel/include/inet.hrl").

-behaviour(gen_fsm).

-export([start_link/2, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export ([layers_receive/1]).

%% FSM States
-export([
    socket/2,
    data/2
]).

-record(state, {
                socket,    						% client socket
                addr,       					% client address
								port,									% server port
								successor,						% server accept function
								queue,								% Denotes if we are using a queue or not
								config = [],					% Starting config
								connect_to_pid = 0,		% connection pid process
								retry_connect_timer,	% timer for retry
								heartbeat_fails = 0,	% Number of failed heartbeat requests
								heartbeat_timeout_ref,% Timer ref for heartbeat
								messages = 0,					% message queue
								reg_name = 0,					% registered name for this process
								connect_mode = ok,		% Connection mode (1)
								hostname = 0					% hostname
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
start_link(Config, Name) ->
   gen_fsm:start_link({local, Name}, ?MODULE, [Config], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).

close_socket() ->
	gen_fsm:send_event(self(), {close_tcp}).	
	
% ONLY FOR TESTING PURPOSES
layers_receive(Msg) ->
	case Msg of
		{data, Socket, Data} -> io:format("~p received in layers_receive: ~p~n", [?MODULE, Data]);
		Else -> io:format("~p received unknown message: ~p~n", [?MODULE, Else])
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
init([Reg_name, Config]) ->
	io:format("Starting ~p~n", [?MODULE]),
	process_flag(trap_exit, true),
	Fun = config:parse(successor, Config), 
	[Port,Queue] = config:fetch_or_default_config([port,queue], Config, ?DEFAULT_CONFIG),
	Hostname = {0,0,0,0},
	IpName = inet_parse:ntoa(my_ip()),
	Name = config:parse_or_default(name, Config, IpName),
	
	global:re_register_name(Reg_name, self()),
	CanUseQueue = converse_queue:maybe_create_queue_table(Queue, Reg_name),
	converse_queue:maybe_wait_for_queue_table(CanUseQueue, Reg_name),
	% erlang:start_timer(0, self(), retry_connect),
	
	{ok, socket, #state{messages = ets:new(messages, []),
	   reg_name = Reg_name, hostname = Hostname, 
	   queue = CanUseQueue, connect_mode = converse_queue:anything_in_queue(CanUseQueue, Reg_name),
	   config = Config, port = Port}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
socket({socket_ready, Socket}, State) when is_port(Socket) ->
	% Now we own the socket
	inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
	case inet:peername(Socket) of
		{ok, {Ip, _Port}} -> {next_state, data, State#state{socket=Socket, addr=Ip}};
		{error, Reason} -> {next_state, data, State#state{socket = Socket}}
	end;	

socket(Other, State) ->
	error_logger:error_msg("State: socket. Unexpected message: ~p\n", [Other]),
	%% Allow to receive async messages
	{next_state, socket, State}.

%% Notification event coming from client
data({data, Data}, #state{addr=Ip, socket=S, successor=Successor} = State) ->
	DataToSend = {data, S, Data},
	?TRACE("Received data", [DataToSend, Successor]),
	case ?debug of
		true -> layers_receive(DataToSend);
		false -> layers:pass(Successor, DataToSend)
	end,
	{next_state, data, State};

data(timeout, State) ->
	error_logger:error_msg("~p Connection timeout - closing.\n", [self()]),
	{stop, normal, State};

data(Data, State) ->
	io:format("~p Ignoring data: ~p\n", [self(), Data]),
	{next_state, data, State, ?DEFAULT_TIMEOUT}.

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
	Msg = case is_binary(Bin) of
		true -> converse_packet:decode(Bin);
		false -> Bin
	end,
	inet:setopts(Socket, [{active, once}]),
	?MODULE:StateName({data, Msg}, State);

handle_info({close_tcp}, StateName, State) ->
	{stop, normal, State};

handle_info({tcp_closed, Socket}, StateName, #state{socket=Socket, addr=Addr, heartbeat_timeout_ref = HeartRef} = State) ->
	case HeartRef of
		undefined -> {stop, normal, State};
		_ ->
			io:format("Closing tcp socket: ~p~n", [Socket]),
		  error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
			New_MessageStore = safe_shutdown(State),
			erlang:start_timer(?RETRY_TIME, self(), retry_connect),
			{next_state, socket, State#state{socket = 0, messages = New_MessageStore, heartbeat_timeout_ref = none}}			
	end;
	
handle_info({socket, Socket}, StateName, State) ->
	cancel_timer(State#state.retry_connect_timer),
	New_ref = erlang:start_timer(?HEARTBEAT_TIMEOUT, self(), heartbeat_timeout),
	NewState = State#state{socket = Socket},
	inet:setopts(Socket, [{active, once}]),
	{next_state, data, NewState#state{heartbeat_timeout_ref = New_ref, heartbeat_fails = 0}};
	% case send_heart(NewState, New_ref) of
	% 	ok -> 
	% 		cancel_timer(State#state.heartbeat_timeout_ref), % cancel timer from last time
	% 		{next_state, data, NewState#state{heartbeat_timeout_ref = New_ref, heart_fails = 0}};
	% 	{error, _} ->
	% 		cancel_timer(New_ref),
	% 		gen_tcp:close(Socket),
	% 		erlang:start_timer(?RETRY_TIME, self(), retry_connect),
	% 		{next_state, socket, State}
	% end;

handle_info({timeout, Ref, retry_connect}, StateName, State) ->
	try_to_kill(State#state.connect_to_pid),
	Self = self(),
	Hostname = State#state.hostname,
	Pid = spawn_link(fun() -> connect_to(Hostname, State#state.port, ?DEFAULT_TIMEOUT, Self) end),
	Ref1 = erlang:start_timer(?RETRY_TIME, self(), retry_connect),
	{next_state, StateName, State#state{connect_to_pid = Pid, retry_connect_timer = Ref1}};

handle_info({'EXIT', Pid, normal}, StateName, State) ->
	{next_state, StateName, State};
	
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
	
safe_shutdown(#state{heartbeat_timeout_ref = HeartRef, messages = Messages} = State) ->
	case HeartRef of
		undefined -> exit(normal, "Undefined heart reference. Unknown connection~n");
		_ ->
			cancel_timer(HeartRef),
			reply_to_all(Messages),
			alarm_handler:set_alarm({{client_lost, State#state.hostname}, []}),
			ets:new(messages, [])
	end.

reply_to_all(MessageStore) -> reply_to_all(ets:first(MessageStore), MessageStore).

reply_to_all('$end_of_table', MessageStore) ->
    ets:delete(MessageStore);
reply_to_all({message,Ref}, MessageStore) ->
	cancel_timer(Ref),
	case ets:lookup(MessageStore, {message, Ref}) of
		{{message, Ref}, null} ->			% Queued ones
			reply_to_all(ets:next(MessageStore), MessageStore);
		{{message, Ref}, From} ->
			gen_fsm:reply(From, {error, link_down}),
			reply_to_all(ets:next(MessageStore), MessageStore);
		% [{{message,#Ref<0.0.0.89>},{<0.39.0>,#Ref<0.0.0.88>}}]
		{{message, _Ref}, {From, Ref}} ->
			% gen_fsm:reply(From, {error, link_down}),
			reply_to_all(ets:next(MessageStore), MessageStore);
		Anything ->
			io:format("Received ~p in reply_to_all~n", [Anything])
	end.
	
try_to_kill(0) -> ok;
try_to_kill(Pid) -> exit(Pid, kill).

cancel_timer(none) -> ok;
cancel_timer(Ref) ->
	erlang:cancel_timer(Ref),
		receive
			{timeout, Ref, _} ->
				ok
			after 0 ->
				ok
	end.
	
connect_to(Hostname, Port, Timeout, Main_server_pid) ->
	case gen_tcp:connect(Hostname, Port, [binary, {active, false}, {packet, raw}], Timeout) of
		{ok, Socket} ->
			ok = gen_tcp:controlling_process(Socket, Main_server_pid),
			Main_server_pid ! {socket, Socket};
		Else ->
			ok
	end.
	
tcp_host({0,0,0,0}) ->
	{ok, Hostname} = inet:gethostname(),
	case inet:gethostbyname(Hostname) of
		{ok, #hostent{h_name = Name}} -> Name;
		{error, _Reason} -> Hostname
	end;
tcp_host(IPAddress) ->
	case inet:gethostbyaddr(IPAddress) of
		{ok, #hostent{h_name = Name}} -> Name;
		{error, _Reason} -> inet_parse:ntoa(IPAddress)
	end.

my_ip() ->
	{ok, Hostname} = inet:gethostname(),
  case inet:gethostbyname(Hostname) of
      {ok, #hostent{h_addr_list = Addrs}} -> erlang:hd(Addrs);
      {error, _Reason} -> Hostname
  end.
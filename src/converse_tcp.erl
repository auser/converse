-module (converse_tcp).
-include ("converse.hrl").
-include_lib("kernel/include/inet.hrl").
-behaviour(gen_fsm).

-export([start_link/1, set_socket/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export ([layers_receive/1]).

%% FSM States
-export([
    socket/2,
    data/2,
		waiting_on_first_heartbeat/2
]).

-record(state, {
		heart_fails = 0,
		heart_ref = none, 
		heartbeat_timeout_ref = none, 
		retry_connect_timer = node,
		socket = 0,												% client socket
    addr = 0,    											% client address
		successor = 0,										% Layers successor
		messages = 0, 										% Message queue
		name = "",												% Name of local process
		reg_name = "",
		secret = false,										% dumb security
		queue = false, 
		connect_mode = ok, 
		config = [],
		connect_to_pid = null, 
		hostname = "", 
		port = 0}).


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
init([Config]) ->
	io:format("in init for ~p with ~p~n", [?MODULE, Config]),
	process_flag(trap_exit, true),

	[Port,Secret,Queue,Successor] = config:fetch_or_default_config([port,secret,queue,successor], Config, ?DEFAULT_CONFIG),
	Hostname = {0,0,0,0},
	IpName = inet_parse:ntoa(my_ip()),
	Name = config:parse_or_default(name, Config, IpName),
	Reg_name = converse_utils:get_registered_name_for_address(tcp, client, {0,0,0,0}),

	global:re_register_name(Reg_name, self()),
	CanUseQueue = converse_queue:maybe_create_queue_table(Queue, Reg_name),
	converse_queue:maybe_wait_for_queue_table(CanUseQueue, Reg_name),
	Secret1 = format_secret(Secret),
	erlang:start_timer(0, self(), retry_connect),
	
	io:format("Starting ~p start_link(~p, ~p, ~p, ~p, ~p) as ~p~n", [?MODULE, Name, Hostname, Port, Secret, Queue, Reg_name]),
	
	{ok, socket, #state{messages = ets:new(messages, []),
	   name = Name, reg_name = Reg_name, hostname = Hostname,
	   queue = CanUseQueue, connect_mode = converse_queue:anything_in_queue(CanUseQueue, Reg_name),
	   secret = Secret1, config = Config, port = Port, successor = Successor}}.

%%-------------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------
% State: waiting_on_first_heartbeat
waiting_on_first_heartbeat({send, Msg, Timeout}, State) ->
	case State#state.queue of
		false -> {reply, {error, not_connected}, waiting_on_first_heartbeat, State};
		true ->
			Name = State#state.reg_name,
			converse_queue:message(Name, {Msg, Timeout}),
			{reply, queued, waiting_on_first_heartbeat, State#state{connect_mode = queue}}
	end;

waiting_on_first_heartbeat({timeout, Ref, heartbeat_timeout}, #state{heartbeat_timeout_ref = Ref} = State) ->
	gen_tcp:close(State#state.socket),
	erlang:start_timer(?RETRY_TIME, self(), retry_connect),
	{reply, {socket, State}, waiting_on_first_heartbeat, State#state{socket = 0, heartbeat_timeout_ref = none}};

waiting_on_first_heartbeat({timeout, Ref, heartbeat_timeout}, State) ->
	{reply, {waiting_on_first_heartbeat, State}, State};
	
waiting_on_first_heartbeat(get_state, State) ->
	{reply, {waiting_on_first_heartbeat, State}, waiting_on_first_heartbeat, State};

waiting_on_first_heartbeat({tcp, Socket, Data}, #state{socket = Socket} = State) ->
	case catch converse_packet:decode(Data) of
		{heart_reply, Ref} when Ref == State#state.heartbeat_timeout_ref ->
			io:format("Received heart_reply in waiting_on_first_heartbeat where heartbeat_timeout_ref == Ref~n", []),
			Next_heartbeat = erlang:start_timer(?HEARTBEAT_PERIOD, self(), send_heart),
			alarm_handler:clear_alarm({client_lost, State#state.hostname}),
			converse_queue:maybe_start_queue_timer(State#state.connect_mode),
			{reply, {data, State#state{heart_ref = Next_heartbeat}}};
		_ ->
			io:format("Received heart_reply in waiting_on_first_heartbeat where heartbeat_timeout_ref != Ref~n", []),
			gen_tcp:close(Socket),
			cancel_timer(State),
			erlang:start_timer(?RETRY_TIME, self(), retry_connect),
			{reply, {socket, State}}
	end;

waiting_on_first_heartbeat({send, Addr, Msg, Timeout}, State) ->
	{reply, {waiting_on_first_heartbeat, State}, waiting_on_first_heartbeat, State};

waiting_on_first_heartbeat({heart_reply, Ref}, State) ->
	if Ref == State#state.heartbeat_timeout_ref ->
			Next_heartbeat = erlang:start_timer(?HEARTBEAT_PERIOD, self(), send_heart),
			alarm_handler:clear_alarm({client_lost, State#state.hostname}),
			converse_queue:maybe_start_queue_timer(State#state.connect_mode),
			{reply, {data, State#state{heart_ref = Next_heartbeat}}};
		false ->
			Socket = State#state.socket,
			gen_tcp:close(Socket),
			cancel_timer(State),
			erlang:start_timer(?RETRY_TIME, self(), retry_connect),
			{reply, {socket, State}}
	end;
	
waiting_on_first_heartbeat(Msg, State) ->
	case catch converse_packet:decode(Msg) of
		{heart_reply, Ref} when Ref == State#state.heartbeat_timeout_ref ->
			Next_heartbeat = erlang:start_timer(?HEARTBEAT_PERIOD, self(), send_heart),
			alarm_handler:clear_alarm({client_lost, State#state.hostname}),
			converse_queue:maybe_start_queue_timer(State#state.connect_mode),
			{reply, {data, State#state{heart_ref = Next_heartbeat}}};
		_ ->
			Socket = State#state.socket,
			gen_tcp:close(Socket),
			cancel_timer(State),
			erlang:start_timer(?RETRY_TIME, self(), retry_connect),
			{reply, {socket, State}}
	end,
	{reply, {waiting_on_first_heartbeat, State}, waiting_on_first_heartbeat, State}.

% State socket	
socket({socket_ready, Socket}, State) when is_port(Socket) ->
	% Now we own the socket
	io:format("Socket ready sent to ~p~n", [?MODULE]),
	inet:setopts(Socket, [{active, once}, {packet, raw}, binary]),
	case inet:peername(Socket) of
		{ok, {Ip, _Port}} -> {next_state, data, State#state{socket=Socket, addr=Ip}};
		{error, Reason} -> 
			error_logger:error_msg("State: socket. Unexpected message: ~p\n", [Reason]),
			{next_state, data, State#state{socket=Socket}}
	end;

socket(Other, State) ->
	error_logger:error_msg("State: socket. Unexpected message: ~p\n", [Other]),
	%% Allow to receive async messages
	{next_state, socket, State}.

%% Notification event coming from client
data({data, Data}, #state{addr=Ip, socket=S, successor=Successor} = State) ->
	io:format("Received ~p in data mode~n", [Data]),
	DataToSend = {data, S, converse_packet:decode(Data)},
	io:format("Received data: ~p => ~p~n", [DataToSend, Successor]),
	
	case ?debug of
		true -> layers_receive(DataToSend);
		false -> layers:pass(Successor, DataToSend)
	end,
	{next_state, data, State};

data({timeout, Ref, retry_connect}, State) ->
	gen_tcp:close(State#state.socket),
	New_Message_Store = controlled_closedown(State),
	erlang:start_timer(?RETRY_TIME, self(), retry_connect),
	{next_state, data, State#state{socket = 0, messages = New_Message_Store, heartbeat_timeout_ref = 0}};

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
handle_info({tcp, Socket, Packet}, StateName, #state{socket=Socket} = State) ->
	% Flow control: enable forwarding of next TCP message
	% case catch converse_packet:decode(Packet) of
	%     {heartbeat, Ref, Checksum} ->
	% 		io:format("Received heartbeat~n", []),
	% 		gen_tcp:send(Socket, converse_packet:encode({heart_reply, Ref}));
	% 	Other ->
			inet:setopts(Socket, [{active, once}]),
			?MODULE:StateName(Packet, State),
	% end,
	{next_state, StateName, State};

handle_info({tcp_closed, Socket}, _StateName,
            #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};

handle_info({timeout, Ref, retry_connect}, socket, State) ->
	maybe_kill(State#state.connect_to_pid),
	Self = self(), Hostname = State#state.hostname,
	Pid = spawn_link(fun() -> connect_to(Hostname, State#state.port, ?DEFAULT_TIMEOUT, Self) end),
	NewRef = erlang:start_timer(?RETRY_TIME, self(), retry_connect),
	{next_state, socket, State#state{connect_to_pid = Pid, retry_connect_timer = NewRef}};
	
handle_info({timeout, Ref, retry_connect}, data, State) ->
	maybe_kill(State#state.connect_to_pid),
	Self = self(), Hostname = State#state.hostname,
	Pid = spawn_link(fun() -> connect_to(Hostname, State#state.port, ?DEFAULT_TIMEOUT, Self) end),
	NewRef = erlang:start_timer(?RETRY_TIME, self(), retry_connect),
	{next_state, socket, State#state{connect_to_pid = Pid, retry_connect_timer = NewRef}};

handle_info({socket, Socket}, StateName, State) ->
	cancel_timer(State#state.retry_connect_timer),
	New_heartbeat_ref = erlang:start_timer(?RETRY_TIME, self(), heartbeat_timeout),
	NewState = State#state{socket = Socket},
	inet:setopts(Socket, [{active, once}]),
	case send_heart(NewState, New_heartbeat_ref) of
		ok ->
			cancel_timer(State#state.heartbeat_timeout_ref),
			{next_state, waiting_on_first_heartbeat, NewState#state{heartbeat_timeout_ref = New_heartbeat_ref, heart_fails = 0}};
		{error, Reason} ->
			cancel_timer(New_heartbeat_ref),
			gen_tcp:close(Socket),
			erlang:start_timer(?RETRY_TIME, self(), retry_connect),
			{next_state, socket, State}
	end;
	
handle_info({'EXIT', Pid, normal}, StateName, State) ->
	if
		State#state.connect_to_pid == Pid ->
			{next_state, StateName, State#state{connect_to_pid = null}};
		true ->
			error_logger:format("Converse. Unexpected EXIT: ~p~n", [Pid]),
			{next_state, StateName, State}
	end;

handle_info(Info, StateName, State) ->
	io:format("Received info: ~p in ~p~n", [Info, StateName]),
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
md5(Ref, Secret) -> erlang:md5(concat_binary([converse_packet:encode(Ref), Secret])).

connect_to(Hostname, Port, Timeout, Main_server_pid) ->
	case gen_tcp:connect(Hostname, Port, [binary, {active, false}, {packet, raw}], Timeout) of
		{ok, Socket} ->
			ok = gen_tcp:controlling_process(Socket, Main_server_pid),
			Main_server_pid ! {socket, Socket};
		Else ->
			ok
	end.
	
maybe_kill(null) -> ok;
maybe_kill(Pid) ->
	io:format("Killing: ~p~n",[Pid]),
	exit(Pid, kill).

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
      {ok, #hostent{h_addr_list = Addrs}} ->  [Addr|_] = Addrs, Addr;
      {error, _Reason} -> Hostname
  end.

send_msg(State, Msg, Ref) when State#state.secret == false ->
	gen_tcp:send(State#state.socket, converse_packet:encode({send, Msg, Ref, []}));
send_msg(State, Msg, Ref) ->
	% io:format("send_msg(~p,~p,~p)~n", [State, Msg, Ref]),
	gen_tcp:send(State#state.socket, converse_packet:encode({send, Msg, Ref, md5({Msg, Ref}, State#state.secret)})).

send_heart(State, Ref) when State#state.secret == false ->
	gen_tcp:send(State#state.socket, converse_packet:encode({heartbeat, Ref, []}));
send_heart(State, Ref) ->
	gen_tcp:send(State#state.socket, converse_packet:encode({heartbeat, Ref, md5(Ref, State#state.secret)})).
	
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
	
cancel_timer(none) -> ok;
cancel_timer(Ref) ->
	erlang:cancel_timer(Ref),
		receive
			{timeout, Ref, _} ->
				ok
			after 0 ->
				ok
	end.
	
controlled_closedown(State) ->
    cancel_timer(State#state.heart_ref),
    cancel_timer(State#state.heartbeat_timeout_ref),
    reply_to_all(State#state.messages),
    alarm_handler:set_alarm({{client_lost, State#state.hostname}, []}),
    ets:new(messages, []).

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
	
format_secret(Secret) when list(Secret) -> list_to_binary(Secret);
format_secret(false) -> false.

check(Ref, false, Checksum) -> true;
check(Ref, Secret, Checksum) -> Checksum == erlang:md5(concat_binary([converse_packet:encode(Ref), Secret])).
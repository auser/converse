-module (converse_tcp).

-include ("converse.hrl").
-include_lib("kernel/include/inet.hrl").

-behaviour(gen_fsm).

%% External exports
-export([start_link/1, start_remote_link/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

% States
-export([
	data/3, 
	socket/3, 
	waiting_on_first_heartbeat/3]).

-export([status/1, send/3, send/2]).
-export ([set_socket/2]).

-export ([tcp_host/1, my_ip/0]).

-record(state, {heart_fails = 0,
		heart_ref = none, 
		heartbeat_timeout_ref = none, 
		retry_connect_timer = node,
		messages = 0, 
		name = "",
		reg_name = "",
		queue = false, 
		connect_mode = ok, 
		socket = 0, 
		config = [],
		connect_to_pid = null, 
		hostname = "", 
		port = 0, 
		secret = null}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% Name is string, Hostname is string, Port is integer, Secret is 'false' or a list,
%% Queue is 'true' or 'false'
start_link(Config) ->
	Reg_name = converse_utils:get_registered_name_for_address(tcp, client, {0,0,0,0}),
  gen_fsm:start_link({local, Reg_name}, ?MODULE, [Config], []).

start_remote_link(Addr, Config) ->
	Reg_name = converse_utils:get_registered_name_for_address(tcp, client, Addr),
  gen_fsm:start_link({local, Reg_name}, ?MODULE, [Config], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) -> gen_fsm:send_event(Pid, {socket_ready, Socket}).

send(Addr, Msg) -> send(Addr, Msg, ?DEFAULT_TIMEOUT).	
send(Addr, Msg, Timeout) ->
	Reg_name = converse_utils:get_registered_name_for_address(tcp, client, Addr),
	case global:whereis_name(Reg_name) of
		undefined ->
			% Spawn a new client connection
			MyLocalClient = converse_utils:get_registered_name_for_address(tcp, client, Addr),
			Reply = gen_fsm:sync_send_event({global, MyLocalClient}, {create_connection, Addr}),
			io:format("Reply from create_connection(~p): ~p~n", [Addr, Reply]),
			gen_fsm:sync_send_event({global, Reg_name}, {send, Msg, Timeout});
		Pid -> 
			gen_fsm:sync_send_event({global, Reg_name}, {send, Msg, Timeout})
	end.
	
status(Addr) ->
	Reg_name = converse_utils:get_registered_name_for_address(tcp, client, Addr),
	gen_fsm:sync_send_event({global, Reg_name}, get_state, 1000).
	
%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}                   
%%----------------------------------------------------------------------
init([Config]) ->
	process_flag(trap_exit, true),

	[Port,Secret,Queue] = config:fetch_or_default_config([port,secret,queue], Config, ?DEFAULT_CONFIG),
	Hostname = {0,0,0,0},
	IpName = inet_parse:ntoa(my_ip()),
	Name = config:parse_or_default(name, Config, IpName),
	Reg_name = converse_utils:get_registered_name_for_address(tcp, client, {0,0,0,0}),

	global:re_register_name(Reg_name, self()),
	CanUseQueue = maybe_create_queue_table(Queue, Reg_name),
	maybe_wait_for_queue_table(CanUseQueue, Reg_name),
	Secret1 = format_secret(Secret),
	erlang:start_timer(0, self(), retry_connect),
	
	io:format("Starting ~p start_link(~p, ~p, ~p, ~p, ~p) as ~p~n", [?MODULE, Name, Hostname, Port, Secret, Queue, Reg_name]),
	
	{ok, socket, #state{messages = ets:new(messages, []),
	   name = Name, reg_name = Reg_name, hostname = Hostname,
	   queue = CanUseQueue, connect_mode = anything_in_queue(CanUseQueue, Reg_name),
	   secret = Secret1, config = Config, port = Port}}.
	

%%----------------------------------------------------------------------
%% Func: StateName/3
%% Called when gen_fsm:sync_send_event/2,3 is invoked.
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------

% If we get a message while we are waiting on the first heartbeat to come through
% we have to queue the command. The server has not yet started nor sent a heartbeat through
% so let's queue the message and let the server come back when it's fully up
waiting_on_first_heartbeat({send, Msg, Timeout}, From, StateData) ->
	case StateData#state.queue of
		false -> {reply, {error, not_connected}, waiting_on_first_heartbeat, StateData};
		true ->
			queue_message(StateData, {Msg, Timeout}),
			{reply, queued, waiting_on_first_heartbeat, StateData#state{connect_mode = queue}}
	end;
waiting_on_first_heartbeat(get_state, From, StateData) ->
	{reply, {waiting_on_first_heartbeat, StateData}, waiting_on_first_heartbeat, StateData};

waiting_on_first_heartbeat({send, Addr, Msg, Timeout}, From, StateData) ->
	{reply, {waiting_on_first_heartbeat, StateData}, waiting_on_first_heartbeat, StateData}.

%%----------------------------------------------------------------------
socket({send, Msg, Timeout}, From, StateData) ->
	io:format("In socket state and received send: ~p~n", [Msg]),
	case StateData#state.queue of
		false -> {reply, {error, not_connected}, socket, StateData};
		true ->
		    queue_message(StateData, {Msg, Timeout}),
		    {reply, queued, socket, StateData#state{connect_mode = queue}}
	end;

socket(get_state, From, StateData) -> {reply, {socket, StateData}, socket, StateData}.

%%----------------------------------------------------------------------
data({send, Msg, Timeout}, From, StateData) when StateData#state.connect_mode == ok ->
	Ref = erlang:start_timer(Timeout, self(), message_timeout),
	case send_msg(StateData, Msg, Ref) of
		ok ->
			ets:insert(StateData#state.messages, {{message, Ref}, From}),
			{next_state, data, StateData};
		{error, Reason} ->
			cancel_timer(Ref),
			{reply, {error, Reason}, data, StateData}
	end;

data({send, Msg, Timeout}, From, StateData) when StateData#state.connect_mode == queue ->
	io:format("Received {send, ~p, ~p} when in connect_mode = queue~n", [Msg, Timeout]),
	queue_message(StateData, {Msg, Timeout}),
	{reply, queued, data, StateData};

data(get_state, From, StateData) -> {reply, {data, StateData}, data, StateData};

% data({create_connection, Addr}, From, State) ->	
% 	io:format("{create_connection, ~p} in ~p~n", [Addr, ?MODULE]),
% 	Pid = ?MODULE:start_remote_link(Addr, State#state.config),
% 	{reply, Pid, State};

data({send, Addr, Msg, Timeout}, From, State) -> {reply, {data, State}, data, State}.
   
%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Called when gen_fsm:send_all_state_event/2 is invoked.
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------
handle_event(Event, StateName, StateData) ->
	{next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Called when gen_fsm:sync_send_all_state_event/2,3 is invoked
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}                    
%%----------------------------------------------------------------------
% handle_sync_event({create_connection, Addr}, From, StateName, State) ->	
% 	io:format("{create_connection, ~p} in ~p~n", [Addr, ?MODULE]),
% 	Pid = ?MODULE:start_remote_link(Addr, State#state.config),
% 	{reply, Pid, StateName, State};

handle_sync_event(Event, From, StateName, StateData) ->
	{reply, ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                         
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% State: socket
%%----------------------------------------------------------------------
handle_info({timeout, Ref, retry_connect}, socket, StateData) ->
	maybe_kill(StateData#state.connect_to_pid),
	Self = self(),
	Hostname = StateData#state.hostname,
	Pid = spawn_link(fun() -> connect_to(Hostname, StateData#state.port, ?DEFAULT_TIMEOUT, Self) end),
	Ref1 = erlang:start_timer(?RETRY_TIME, self(), retry_connect),
	{next_state, socket, StateData#state{connect_to_pid = Pid, retry_connect_timer = Ref1}};

handle_info({socket, Socket}, socket, StateData) ->
	cancel_timer(StateData#state.retry_connect_timer),
	New_ref = erlang:start_timer(?HEARTBEAT_TIMEOUT, self(), heartbeat_timeout),
	NewStateData = StateData#state{socket = Socket},
	inet:setopts(Socket, [{active, true}]),
	case send_heart(NewStateData, New_ref) of
		ok -> 
			cancel_timer(StateData#state.heartbeat_timeout_ref), % cancel timer from last time
			{next_state, waiting_on_first_heartbeat, NewStateData#state{heartbeat_timeout_ref = New_ref, heart_fails = 0}};
		{error, _} ->
			cancel_timer(New_ref),
			gen_tcp:close(Socket),
			erlang:start_timer(?RETRY_TIME, self(), retry_connect),
			{next_state, socket, StateData}
	end;


%%----------------------------------------------------------------------
%% State: waiting_on_first_heartbeat
%%----------------------------------------------------------------------
%% Waiting for first heartbeat. Received timeout which is the current attempt
handle_info({timeout, Ref, heartbeat_timeout}, waiting_on_first_heartbeat, #state{heartbeat_timeout_ref = Ref} = StateData) ->
	gen_tcp:close(StateData#state.socket),
	erlang:start_timer(?RETRY_TIME, self(), retry_connect),
	{next_state, socket, StateData#state{socket = 0, heartbeat_timeout_ref = none}};

%% Waiting for first heartbeat. Received random timeout which could just
%% be a none cancelled timer from a previous attempt.
%% Continue to wait for the real one.
handle_info({timeout, Ref, heartbeat_timeout}, waiting_on_first_heartbeat, StateData) ->
    {next_state, waiting_on_first_heartbeat, StateData};

%% received some data
handle_info({tcp, Socket, Data}, waiting_on_first_heartbeat, #state{socket = Socket} = StateData) ->
	case catch converse_packet:decode(Data) of
		{heart_reply, Ref} when Ref == StateData#state.heartbeat_timeout_ref ->
			Next_heart = erlang:start_timer(?HEARTBEAT_PERIOD, self(), send_heart),
			alarm_handler:clear_alarm({client_lost, StateData#state.hostname}),
			maybe_start_queue_timer(StateData#state.connect_mode),
			{next_state, data, StateData#state{heart_ref = Next_heart}};
		_ ->
			gen_tcp:close(Socket),
			cancel_timer(StateData#state.heartbeat_timeout_ref),
			erlang:start_timer(?RETRY_TIME, self(), retry_connect),
			{next_state, socket, StateData}
	end;

%%----------------------------------------------------------------------
%% State: data
%%----------------------------------------------------------------------
%% Received Nth heartbeat timeout in data phase which means we 
%% should close and start again.
handle_info({timeout, Ref, heartbeat_timeout}, data, StateData) when StateData#state.heart_fails >= ?MAX_FAIL_ATTEMPTS ->
	gen_tcp:close(StateData#state.socket),
	New_MessageStore = controlled_closedown(StateData),
	erlang:start_timer(?RETRY_TIME, self(), retry_connect),
	{next_state, socket, StateData#state{socket = 0, messages = New_MessageStore, heartbeat_timeout_ref = none}};

%% Received non final heartbeat timeout in data phase, increment counter.
handle_info({timeout, Ref, heartbeat_timeout}, data, StateData) ->
	Heart_fails = StateData#state.heart_fails,
	{next_state, data, StateData#state{heart_fails = Heart_fails + 1}};

%% Time to send a new heartbeat.
handle_info({timeout, Ref, send_heart}, data, StateData) ->
	cancel_timer(StateData#state.heartbeat_timeout_ref), % Just in case
	New_ref = erlang:start_timer(?HEARTBEAT_TIMEOUT, self(), heartbeat_timeout),
	Next_heart = erlang:start_timer(?HEARTBEAT_PERIOD, self(), send_heart),
	case send_heart(StateData, New_ref) of
		ok -> {next_state, data, StateData#state{heartbeat_timeout_ref = New_ref, heart_ref = Next_heart}};
	{error, _} ->
		cancel_timer(New_ref),
		Heart_fails = StateData#state.heart_fails,
		{next_state, data, StateData#state{heart_fails = Heart_fails + 1, heartbeat_timeout_ref = null, heart_ref = Next_heart}}
	end;

%% Received timeout for a sent command.
handle_info({timeout, Ref, message_timeout}, data, StateData) ->
	Messages = StateData#state.messages,
	case ets:lookup(Messages, {message, Ref}) of
		[{{message, Ref}, Reply_to}] ->
			gen_fsm:reply(Reply_to, {error, timed_out}),
			ets:delete(Messages, {message, Ref}),
			{next_state, data, StateData};
		[] ->
			{next_state, data, StateData}
	end;

%% Received timeout for a sent command which had been originally queued.
handle_info({timeout, Ref, queued_message_timeout}, data, StateData) ->
	Messages = StateData#state.messages,
	case ets:lookup(Messages, {message, Ref}) of
		[{{message, Ref}, Reply_to}] ->
			ets:delete(Messages, {message, Ref}),
			{next_state, data, StateData};
		[] -> {next_state, data, StateData}
	end;

%% Received timeout telling us to send the next queued command.
%% Dirty commands are fine here as by definition we can only act on
%% this one record and no-one else is doing so
handle_info({timeout, Ref, catchup_timeout}, data, StateData) ->
	Queue_tab = StateData#state.reg_name,
	case mnesia:dirty_first(Queue_tab) of
		'$end_of_table' ->
			{next_state, data, StateData#state{connect_mode = ok}};
		Key ->
			[{_, Now, {Msg, Timeout}}] = mnesia:dirty_read(Queue_tab, Key),
			mnesia:dirty_delete(Queue_tab, Key),
			New_ref = erlang:start_timer(Timeout, self(), queued_message_timeout),
			erlang:start_timer(?SLOWDOWN_INTERVAL, self(), catchup_timeout),
			case send_msg(StateData, Msg, New_ref) of
				ok ->
					ets:insert(StateData#state.messages, {{message, New_ref}, null}),
					{next_state, data, StateData};
				{error, Reason} ->
					cancel_timer(New_ref),	% This one has had it - failed first time so queued
					{next_state, data, StateData} % and now a victim of this!
			end
	end;

handle_info({tcp, Socket, Data}, data, #state{socket = Socket} = StateData) ->
	Messages = StateData#state.messages,
	case catch converse_packet:decode(Data) of
		{reply, Ref, Reply} ->
			case ets:lookup(Messages, {message, Ref}) of
				[{{message, Ref}, null}] ->		% Previously queued
					ets:delete(Messages, {message, Ref}),
					cancel_timer(Ref),
					{next_state, data, StateData};
			[{{message, Ref}, Reply_to}] ->
				gen_fsm:reply(Reply_to, Reply),
				ets:delete(Messages, {message, Ref}),
				cancel_timer(Ref),
				{next_state, data, StateData};
			[] ->
				{next_state, data, StateData}
			end;
		{heart_reply, Ref} when Ref == StateData#state.heartbeat_timeout_ref ->
			cancel_timer(Ref),
			{next_state, data, StateData#state{heart_fails = 0}};
		Else ->
			error_logger:format("converse Unknown Packet received~p~n", [Else]),
			{next_state, data, StateData}
	end;



%%----------------------------------------------------------------------
%% State: all states
%%----------------------------------------------------------------------
handle_info({tcp, _, _}, StateName, StateData) -> % Ignore Packets received in other states
	{next_state, StateName, StateData};

handle_info({tcp_closed, Socket}, StateName, #state{socket = Socket} = StateData) ->
	New_MessageStore = controlled_closedown(StateData),
	erlang:start_timer(?RETRY_TIME, self(), retry_connect),
	{next_state, socket, StateData#state{socket = 0, messages = New_MessageStore, heartbeat_timeout_ref = none}};

handle_info({tcp_error, Socket, Reason}, StateName, #state{socket = Socket} = StateData) ->
	error_logger:format("Converse. Tcp_Error: ~p~n", [Reason]),
	{next_state, StateName, StateData};

%% exits from the connect_to processes
handle_info({'EXIT', Pid, normal}, StateName, StateData) ->
	if
		StateData#state.connect_to_pid == Pid ->
			{next_state, StateName, StateData#state{connect_to_pid = null}};
		true ->
			error_logger:format("Converse. Unexpected EXIT: ~p~n", [Pid]),
			{next_state, StateName, StateData}
	end;

handle_info(Info, StateName, StateData) ->
	error_logger:format("Converse. Unexpected Info: ~p~n", [Info]),
	{next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StatData) ->
	ok.

%%----------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%%----------------------------------------------------------------------
code_change(OldVsn, StateName, StateData, Extra) ->
	{ok, StateName, StateData}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

cancel_timer(none) -> ok;
cancel_timer(Ref) ->
	erlang:cancel_timer(Ref),
		receive
			{timeout, Ref, _} ->
				ok
			after 0 ->
				ok
	end.


controlled_closedown(StateData) ->
    cancel_timer(StateData#state.heart_ref),
    cancel_timer(StateData#state.heartbeat_timeout_ref),
    reply_to_all(StateData#state.messages),
    alarm_handler:set_alarm({{client_lost, StateData#state.hostname}, []}),
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



md5(Ref, Secret) -> erlang:md5(concat_binary([converse_packet:encode(Ref), Secret])).

maybe_create_queue_table(false, _) -> false;
maybe_create_queue_table(true, Name) ->
	case catch mnesia:table_info(Name, storage_type) of
	{'EXIT', {aborted, {no_exists, Name, storage_type}}} ->
		case mnesia:create_table(Name, [{disc_copies, [node()]}, 
																		{attributes, record_info(fields, converse_message_queue)}, 
																		{type, ordered_set}]) of
			{atomic, ok} -> true;
		Else ->
			error_logger:format("converse app cannot create queue table", []),
			false
		end;
	Ok ->
		true
	end.

send_msg(StateData, Msg, Ref) when StateData#state.secret == false ->
	gen_tcp:send(StateData#state.socket, converse_packet:encode({send, Msg, Ref, []}));
send_msg(StateData, Msg, Ref) ->
	% io:format("send_msg(~p,~p,~p)~n", [StateData, Msg, Ref]),
	gen_tcp:send(StateData#state.socket, converse_packet:encode({send, Msg, Ref, md5({Msg, Ref}, StateData#state.secret)})).

send_heart(StateData, Ref) when StateData#state.secret == false ->
	gen_tcp:send(StateData#state.socket, converse_packet:encode({heartbeat, Ref, []}));
send_heart(StateData, Ref) ->
	gen_tcp:send(StateData#state.socket, converse_packet:encode({heartbeat, Ref, md5(Ref, StateData#state.secret)})).

format_secret(Secret) when list(Secret) -> list_to_binary(Secret);
format_secret(false) -> false.

maybe_start_queue_timer(ok) -> ok;
maybe_start_queue_timer(queue) ->
	erlang:start_timer(?SLOWDOWN_INTERVAL, self(), catchup_timeout).

queue_message(StateData, Message) ->
	Name = StateData#state.reg_name,
	mnesia:dirty_write(Name, {Name, now(), Message}).

%% This function is spawned as a separate process to wait for the
%% connection to be established. This is so that clients can still
%% queue their commands which we are waiting in gen_tcp:connect/4
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

maybe_wait_for_queue_table(false, _) -> ok;
maybe_wait_for_queue_table(true, Reg_name) ->
	mnesia:wait_for_tables([Reg_name], infinity).

anything_in_queue(false, Reg_name) -> ok;
anything_in_queue(true, Reg_name) ->
	case mnesia:dirty_first(Reg_name) of
		'$end_of_table' -> ok;
		Else -> queue
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
      {ok, #hostent{h_addr_list = Addrs}} ->  [Addr|_] = Addrs, Addr;
      {error, _Reason} -> Hostname
  end.
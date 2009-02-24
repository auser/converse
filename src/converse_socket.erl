-module (converse_socket).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/4, get_connection/1, worker/4]).

-behaviour(gen_server).

-include_lib("kernel/include/inet.hrl").

% Internal state for this socket process
-record(state,{	
								listen_pid,		% Pid of Listener
								listen_socket,		% Listener Socket
								socket = undefined,	% Socket ref
								successor = null,
								secret = null}).		% Shared Secret

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(ListenPid, ListenSocket, Secret, Successor) ->
    gen_server:start_link(converse_socket, {ListenPid, ListenSocket, Secret, Successor},[]).

get_connection(Pid) -> gen_server:cast(Pid, get_conn).

%%%----------------------------------------------------------------------
%%% Callbacks
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init({ListenPid, ListenSocket, Secret, Successor}) ->
	{ok, #state{listen_pid = ListenPid, listen_socket = ListenSocket, secret = Secret, successor = Successor}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Request,From,State) ->
	{reply,ok,State}.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(get_conn, State) ->
	case catch gen_tcp:accept(State#state.listen_socket) of
	{error, closed} -> 	{stop, {error, accept_failed}, State};
	{error, Reason} -> 	{stop, {error, accept_failed}, State};
	{'EXIT', Reason} -> {stop, {error, accept_failed}, State};
	{ok, Socket} ->		
		converse_listener:create(State#state.listen_pid, self()),
		{noreply, State#state{socket = Socket}}
	end;

handle_cast(_Reply ,State) ->
	{noreply, State}.
%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({tcp, Socket, Packet}, #state{successor = Successor} = State) ->
	case catch converse_packet:decode(Packet) of
		{heartbeat, Ref, Checksum} ->
			case check(Ref, State#state.secret, Checksum) of
				true ->
					gen_tcp:send(Socket, converse_packet:encode({heart_reply, Ref})),
					{noreply, State};
				false ->
					{noreply, State}
			end;
		{send, Msg, Ref, Checksum} ->
			case check({Msg, Ref}, State#state.secret, Checksum) of
				true ->
					Pid = spawn_link(?MODULE, worker, [Successor, Msg, Ref, Socket]),
					{noreply, State};
				false -> {noreply, State}
			end;
		Else ->
			io:format("Socket Received Else: ~p~n",[Else]),
			{noreply, State}
	end;

handle_info({tcp_closed, Socket}, State) ->
    {stop, converse_closed, State};

handle_info({tcp_error, Socket, Reason}, State) ->
    gen_tcp:close(State#state.socket),
    {stop, converse_skt_error, State};


handle_info({inet_async, ListSock, Ref, {ok, CliSocket}}, #state{socket=ListSock} = State) ->
	try
		io:format("Received inet_async in ~p~n", [?MODULE])
	catch exit:Why ->
		error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
		{stop, Why, State}
	end;

handle_info({inet_async, ListSock, Ref, Error}, #state{socket=ListSock} = State) ->
	error_logger:error_msg("Error in socket tcp_acceptor: ~p.\n", [Error]),
	{stop, Error, State};

handle_info({'EXIT', _ListSock, _Ref, Error}, State) ->
	error_logger:error_msg("Error in socket tcp_acceptor: ~p.\n", [Error]),
	{noreply, State};

handle_info(Info, State) ->
	io:format("Received info", [Info]),
  {noreply, State}.


%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, #state{socket = undefined}) ->
    ok;
terminate(Reason, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.	

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% Spawned to do the work being asked for. Should normally exit with
%% reason normal so we don't upset the main gen_server.
worker(Successor, Msg, Ref, Socket) ->
	Reply = case catch layers:pass(Successor, Msg) of
		{error, Reason} ->
			io:format("Error in layers pass: ~p~n", [Reason]),
			error;
		Val -> Val
	end,
	io:format("Reply: ~p~n", [Reply]),
	ok = gen_tcp:send(Socket, converse_packet:encode({reply, Ref, Reply})).
	    
check(Ref, false, Checksum) ->
    true;
check(Ref, Secret, Checksum) -> Checksum == erlang:md5(concat_binary([converse_packet:encode(Ref), Secret])).

get_host(Address) ->
	case inet:gethostbyaddr(Address) of
		{ok, Hostent} -> {ok, Hostent#hostent.h_name};
		{error, Reason} -> error
	end.
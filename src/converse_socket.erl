-module (converse_socket).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/4, get_connection/1, worker/4]).

-behaviour(gen_server).

-include_lib("kernel/include/inet.hrl").

% Internal state for this socket process
-record(state,{	
								listen_pid,		% Pid of Listener
								listen_socket,		% Listener Socket
								socket = undefined,	% Socket ref
								successor = null,
								config = 0,
								tcp_acceptor,
								secret = null}).		% Shared Secret

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(ListenPid, ListenSocket, Secret, Successor) ->
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
	io:format("Received tcp socket~n"),
	case catch converse_packet:decode(Packet) of
		{heartbeat, Ref, Checksum} ->
			case check(Ref, State#state.secret, Checksum) of
				true ->
					gen_tcp:send(Socket, converse_packet:encode({heart_reply, Ref})),
					{noreply, State};
				false ->
					{noreply, State}
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

handle_info({inet_async, ListSock, Ref, {ok, CliSocket}}, #state{listen_socket=ListSock, config=Config} = State) ->
	try
		case set_sockopt(ListSock, CliSocket) of
			ok -> ok;
			{error, Reason} -> 
				exit({set_sockopt, Reason})
	  end,

    Pid = case converse_app:start_tcp_client(Config) of
			{ok, P} -> P;
			{error, {already_started, P}} -> P;
			{error, R} -> 
				io:format("Error starting tcp client ~p~n", [R]),
				exit({error, R})
		end,
    gen_tcp:controlling_process(CliSocket, Pid),
    converse_tcp:set_socket(Pid, CliSocket),

    %% Signal the network driver that we are ready to accept another connection
    case prim_inet:async_accept(ListSock, -1) of
	    {ok, NewRef} -> ok;
			{error, NewRef} -> exit({async_accept, inet:format_error(NewRef)})
		end,
		{noreply, State#state{tcp_acceptor=NewRef}}

	catch exit:Why ->
		error_logger:error_msg("Error in async accept: ~p.\n", [Why]),
		{stop, Why, State}
	end;

handle_info({inet_async, ListSock, Ref, Error}, #state{listen_socket=ListSock, tcp_acceptor=Ref} = State) ->
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
	
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock, [active, nodelay, keepalive, delay_send, priority, tos]) of
    {ok, Opts} ->
        case prim_inet:setopts(CliSocket, Opts) of
        ok    -> ok;
        Error -> gen_tcp:close(CliSocket), Error
        end;
    Error ->
        gen_tcp:close(CliSocket), Error
    end.
-module (converse_listener).
-include ("converse.hrl").
-behaviour(gen_server).

%% External API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
                listener,
                tcp_acceptor, 
								config,
								% udp_acceptor,
								receive_function
               }).

%%--------------------------------------------------------------------
%% @spec (Port::integer(), Module) -> {ok, Pid} | {error, Reason}
%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(Config) ->	
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------
init([Config]) ->
    process_flag(trap_exit, true),
		
		DefaultPort = converse_utils:safe_integer(config:parse(port, Config)),		
		ReceiveFunction = config:parse(successor, Config),
		Port = converse_utils:get_app_env(listen_port, DefaultPort),

    Opts = [binary, {packet, 2}, {reuseaddr, true}, {keepalive, true}, {backlog, 30}, {active, false}],
		?TRACE("Starting converse_listener with config ~p~n", [Config]),
				
    case gen_tcp:listen(Port, Opts) of
    {ok, Sock} ->
        %%Create first accepting process
        {ok, Ref} = prim_inet:async_accept(Sock, -1),
				?TRACE("Listening on port with Opts", [Port, ReceiveFunction, Opts]),
        {ok, #state{listener = Sock,
                    tcp_acceptor = Ref,
										config = Config,
										% udp_acceptor = UdpAcceptor,
										receive_function=ReceiveFunction}};
    {error, Reason} ->
        {stop, Reason}
    end.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call({send, {Address, Port}, Msg}, _From, State) ->
	case open_socket({Address, Port}) of
		{ok, Socket} ->
			case send_to_open(Socket, {keyreq}) of
				{error, Reason} ->
					io:format("Error in initiating call: ~p~n", [Reason]),
					{error, Reason};
				_Anything ->
					send_to_open(Socket, Data)
			end;
		{error, Reason} -> {error, Reason}
	end,
	{reply, ok, State};
	
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------	
handle_cast(_Msg, State) ->
	{noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------	
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state{listener=ListSock, receive_function=RecFun, tcp_acceptor=Ref} = State) ->
	try
		case set_sockopt(ListSock, CliSocket) of
			ok -> ok;
			{error, Reason} -> 
				exit({set_sockopt, Reason})
	  end,

    {ok, Pid} = converse:start_tcp_client(RecFun),
    gen_tcp:controlling_process(CliSocket, Pid),
    tcp_app_fsm:set_socket(Pid, CliSocket),

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

handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, tcp_acceptor=Ref} = State) ->
	error_logger:error_msg("Error in socket tcp_acceptor: ~p.\n", [Error]),
	{stop, Error, State};

handle_info({'EXIT', _ListSock, _Ref, Error}, State) ->
	error_logger:error_msg("Error in socket tcp_acceptor: ~p.\n", [Error]),
	{noreply, State};

handle_info({change_receiver, Fun}, #state{receive_function=RecFun} = State) ->
	NewState = case RecFun =:= Fun of
		false -> State#state{receive_function=Fun};
		true -> State
	end,
	?TRACE("Changed receive function to ~p.\n", [NewState]),
	{noreply, NewState};

handle_info(Info, State) ->
	?TRACE("Received info", [Info]),
  {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Internal functions
%%%------------------------------------------------------------------------

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

open_socket({Address, Port}) ->
	case gen_tcp:connect(Address, Port, [{packet, 2}]) of
		{error, Reason} ->
			io:format("Error ~p~n", [Reason]),
			{error, Reason};
		{ok, Socket} ->
			{ok, Socket}
	end.
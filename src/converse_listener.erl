-module (converse_listener).
-include ("converse.hrl").
-behaviour(gen_server).

%% External API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {
                listener,       % Listening socket
                acceptor,       % Asynchronous acceptor's internal reference
                module,         % FSM handling module
                sup_name,       % Supervisor's registered name
								config,					% Local config
								successor				% Layer's successor
               }).

%%--------------------------------------------------------------------
%% @spec (ListenerSupName, ConnectionSupName, Port::integer(), Module) -> Result
%%          ListenerSupName   = atom()
%%          ConnectionSupName = atom()
%%          Result            = {ok, Pid} | {error, Reason}
%%
%% @doc Called by a supervisor to start the listening process.
%%      `ListenerSupName' is the name given to the listening process
%%      supervisor.  `ConnectionSupName' is the name given to the
%%      supervisor of spawned client connections.  The listener will be
%%      started on a given `Port'.  `Module' is the FSM implementation
%%      of the user-level protocol.  This module must implement
%%      `set_socket/2' function.
%% @end
%%----------------------------------------------------------------------
start_link(ListenerSupName, ConnectionSupName, Module, Config) when is_atom(Module) ->
    gen_server:start_link({local, ListenerSupName}, ?MODULE, [ConnectionSupName, Module, Config], []).

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
%% @private
%%----------------------------------------------------------------------
init([ConnSupName, Module, Config]) ->
    process_flag(trap_exit, true),
		[Port,Successor,SockOpts] = config:fetch_or_default_config([port,successor,sock_opts], Config, ?DEFAULT_CONFIG),
    try
        case listen_socket(Port, SockOpts) of
        {ok, LSock}          -> ok;
        Error                -> LSock = none, throw(Error)
        end,

        %%Create first accepting process
        {ok, Ref} = prim_inet:async_accept(LSock, -1),
        {ok, #state{listener 	= LSock,
                    acceptor 	= Ref,
                    module   	= Module,
										config 		= Config,
                    sup_name 	= ConnSupName
										}}
    catch What ->
        {stop, What}
    end.

listen_socket(Port, Opts) when is_integer(Port) ->
	Backlog = 30, gen_tcp:listen(Port, Opts ++ [{backlog, Backlog}]);

listen_socket(Filename, Opts) when is_list(Filename) ->
	try
	    case unixdom_drv:start() of
	    {ok, DrvPort}    -> ok;
	    {error, Reason}  -> DrvPort = undefined, throw({error, Reason});
	    {'EXIT', Reason} -> DrvPort = undefined, throw({error, Reason})
	    end,
	    file:delete(Filename),
	    case unixdom_drv:listen(DrvPort, Filename, Opts) of
	    {ok, LSock} ->
	        {ok, DrvPort, LSock};
	    Error ->
	        Error
	    end
	catch What ->
	    What
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
            #state{listener=ListSock, acceptor=Ref, module=Module, sup_name=SupName} = State) ->
    case set_sockopt(ListSock, CliSocket) of
    ok ->
        %% New client connected - spawn a new process using the simple_one_for_one
        %% supervisor.
        {ok, Pid} = converse_app:start_client(SupName),
        gen_tcp:controlling_process(CliSocket, Pid),
        %% Instruct the new FSM that it owns the socket.
        Module:set_socket(Pid, CliSocket),
        {ok, NewRef} = prim_inet:async_accept(ListSock, -1),
        {noreply, State#state{acceptor=NewRef}};
    {error, Reason} ->
        error_logger:error_msg("Error setting socket options: ~p.\n", [Reason]),
        {stop, Reason, State}
    end;

handle_info({inet_async, ListSock, Ref, Error}, #state{listener=ListSock, acceptor=Ref} = State) ->
    error_logger:error_msg("Error in socket acceptor: ~p.\n", [Error]),
    {stop, Error, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, #state{listener=LSock}) ->
	gen_tcp:close(LSock).

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

%% Taken from prim_inet.  We are merely copying some socket options from the
%% listening socket to the new client socket.
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
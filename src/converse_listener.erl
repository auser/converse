-module (converse_listener).
-include ("converse.hrl").

-behaviour(gen_server).

-record(state,{	listen_socket = [],
								accept_pid = null,
								tcp_acceptor = null,
								successor = undefined,
								secret = 0,
								config = 0}).

%% External API
-export([start_link/1,create/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%%--------------------------------------------------------------------
%% @spec (Port::integer(), Module) -> {ok, Pid} | {error, Reason}
%
%% @doc Called by a supervisor to start the listening process.
%% @end
%%----------------------------------------------------------------------
start_link(Config) ->
  Name = converse_utils:get_registered_name_for_address(tcp, listener, {0,0,0,0}),
  gen_server:start_link({local, Name}, ?MODULE, [Config], []).
 
%% Access to this server from socket servers
create(ServerPid, Pid) ->
  gen_server:cast(ServerPid, {create, Pid}).
				
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
		
		[Secret,Opts,Port,Successor] = config:fetch_or_default_config([secret,sock_opts,port,successor], Config, ?DEFAULT_CONFIG),
		RealName = converse_utils:get_registered_name_for_address(tcp, client, {0,0,0,0}),
		
    case gen_tcp:listen(Port, Opts) of
    {ok, Sock} ->
	      Pid = case global:whereis_name(RealName) of
	      	undefined -> converse_socket:start_link(self(), Sock, Secret, Successor); % launch new converse_socket acceptor
					P -> P% converse_socket exists for this connection already
	      end,
		    {ok, #state{listen_socket = Sock,
										tcp_acceptor = Pid,
										config = Config,
                    successor = Successor}};
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
handle_cast({create,Pid}, State) ->
    {ok, NewPid} = converse_socket:start(self(), State#state.listen_socket, State#state.secret, State#state.successor),
    converse_socket:get_connection(NewPid),
    {noreply, State#state{accept_pid = NewPid}};
	
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
handle_info(Info, State) ->
	io:format("Received info", [Info]),
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
    gen_tcp:close(State#state.listen_socket),
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
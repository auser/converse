-module (converse_listener).
-include ("converse.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([create/2]).

-behaviour(gen_server).

-record(state,{	listen_socket = [],		% Listener socket reference
								accept_pid = null,		% Pid of current acceptor
								tcp_acceptor = null, 	% Acceptor Ref
								successor = undefined,
								secret = null}).			% Shared Secret
%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(Config) ->
	Name = converse_utils:get_registered_name_for_address(tcp, listener, {0,0,0,0}),
	gen_server:start_link({local, Name}, ?MODULE, [Config], []).


%% Access to this server from socket servers
create(ServerPid, Pid) ->
	gen_server:cast(ServerPid, {create, Pid}).


%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init([Config]) ->
	process_flag(trap_exit, true),
	[Port,Secret,Successor] = config:fetch_or_default_config([port,secret,successor], Config, ?DEFAULT_CONFIG),
	RealSecret = format_secret(Secret),
	
	case gen_tcp:listen(Port, ?DEFAULT_SOCKET_OPTS) of
		{ok, ListenSocket} ->
			%%Create first accepting process
			{ok, Pid} = converse_socket:start(self(), ListenSocket, RealSecret, Successor),
			converse_socket:get_connection(Pid),
			{ok, #state{listen_socket = ListenSocket, accept_pid = Pid, secret = RealSecret}};
		{error, Reason} ->
			{stop, Reason}
	end.
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
handle_cast({create,Pid}, State) ->
    {ok, NewPid} = converse_socket:start(self(), State#state.listen_socket, State#state.secret, State#state.successor),
    converse_socket:get_connection(NewPid),
    {noreply, State#state{accept_pid = NewPid}}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
% 
handle_info({'EXIT', Pid, {error, accept_failed}}, State) ->
    create(self(), self()),			% Start off new acceptor as listen socket is still open
    {noreply,State};

% normal shutdown of socket process
handle_info({'EXIT', Pid, normal}, State) ->
    {noreply,State};

% unexpected shutdown of current acceptor
handle_info({'EXIT', Pid, Reason}, State) when State#state.accept_pid == Pid ->
%    io:format("Acceptor quit: ~p~n",[Reason]),
    create(self(), self()),
    {noreply,State};

handle_info({'EXIT', Pid, converse_closed}, State) ->
	{noreply, State};

handle_info(Info, State) ->
	io:format("~p. Unexpected info: ~p~n",[?MODULE, Info]),
	{noreply,State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason,State) ->
    gen_tcp:close(State#state.listen_socket),
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

format_secret(Secret) when list(Secret) -> list_to_binary(Secret);
format_secret(false) -> false.
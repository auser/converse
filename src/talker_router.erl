%%%%%
% The router handles holding the list of connections,
% registering connections, removing connections
%%%%%

-module (talker_router).
-author ("Ari Lerner").
-include("talker.hrl").

-behaviour(gen_server).

%% API
-export ([	start_link/0,
			send/2,
			register_connection/4,
			register_connection_with_info/5, 
			unregister_connection/2, 
			get_all_connections/0,
			get_local_address_port/0
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Macros
-define(SERVER, ?MODULE).


%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

% Send pass the Message to the node at Address and Port
send({Address, Port}, Message) ->
	case ?DB:lookup(?MODULE, {Address, Port}) of
		[{{Address, Port}, Node}] -> 
			Pid = Node#node.pid, Address = Node#node.address,
			talker_connection:send(Address, Pid, Message), ok;
		[] ->
			NewNode = #node{address = Address, port = Port},
			gen_server:call(?MODULE, {send, NewNode, Message}, ?TIMEOUT)
	end.

get_all_connections() ->
	handle_get_all_connections().

register_connection(Address, Port, Pid, Socket) ->
	register_connection_with_info(Address, Port, Pid, Socket, {}).

register_connection_with_info(Address, Port, Pid, Socket, Tuple) ->
	gen_server:call(?SERVER, {register_connection, Address, Port, Pid, Socket, Tuple}, ?TIMEOUT).

unregister_connection(Address, Port) ->
	gen_server:call(?SERVER, {unregister_connection, Address, Port}, ?TIMEOUT).

get_local_address_port() ->
	gen_server:call(?SERVER, {get_local_address_port}, ?TIMEOUT).
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->	
	process_flag(trap_exit, true),
	?DB:new(?MODULE, [set, protected, named_table]),
	{ok, Hostname} = inet:gethostname(),
    {ok, HostEntry} = inet:gethostbyname(Hostname),
    [LocalIP|_] = HostEntry#hostent.h_addr_list,
	LocalPort = ?DEFAULT_PORT,
	NewState = #node{address = LocalIP, port = LocalPort},
    {ok, NewState}.

handle_call({send, Node, Message}, _From, State) ->
	handle_forward_message(Node, Message, State);
	
handle_call({register_connection, Address, Port, Pid, Socket, Tuple}, _From, State) ->
	handle_register_connection(Address, Port, Pid, Socket, Tuple, State);

handle_call({unregister_connection, Address, Port}, _From, State) ->
	handle_unregister_connection(Address, Port, State);

handle_call({get_local_address_port}, _From, State)	->
	handle_get_local_address_port(State);
	
handle_call(Request, _From, State) ->
	io:format("Received request ~p in talk_router~n", [Request]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------
%% API
%%------
handle_register_connection(Address, Port, Pid, Socket, Tuple, State) ->
	case ?DB:lookup(?MODULE, {Address, Port}) of
		[{{Address, Port}, _}] ->
			io:format("Already registered ~p~n", [Address]),
			{reply, dups, State};
		[] ->
			NewNode = State#node{address=Address, port=Port, pid=Pid, socket=Socket, tuple=Tuple},
			?DB:insert(?MODULE, {{Address, Port}, NewNode}),
			{reply, ok, State}
	end.
	
handle_unregister_connection(Address, Port, State) ->
	?DB:delete(?MODULE, {Address, Port}),
	{reply, ok, State}.

handle_forward_message({{_IP1, _IP2, _IP3, _IP4} = Address, Port}, Message, State) ->	
	FindNode = State#node{address = Address, port = Port},
	handle_forward_message(FindNode, Message, State);

handle_forward_message({Node, Message}, _, State) ->
	Address = Node#node.address, 
	Port = Node#node.port,
	{MyAddress, MyPort} = get_local_address_port(),
	case (Address =:= MyAddress andalso Port =:= MyPort) of
		true -> self() ! Message;
		_ ->
			handle_remote_forward_message(Address, Port, Message, State)
	end.
	
handle_remote_forward_message(Address, Port, Message, State) ->
	case ?DB:lookup(?MODULE, {Address, Port}) of
		[{{Address, Port}, Node}] ->
			talker_connection:send(Node, Message),
			{reply, ok, State};
		[] ->
			{reply, unknown_node, State};
		Unknown ->
			io:format("Unknown response to handle_remote_forward_message: ~p~n", [Unknown]),
			{reply, ok, State}
	end.


handle_get_all_connections() ->
	?DB:tab2list(?MODULE).

handle_get_local_address_port(State) ->
	LocalAddress = State#node.address,
	LocalPort = State#node.port,
	{reply, {LocalAddress, LocalPort}, State}.

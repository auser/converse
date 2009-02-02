%%%%%
% The router handles holding the list of connections,
% registering connections, removing connections
%%%%%

-module (converse_router).
-author ("Ari Lerner").
-include("converse.hrl").

-behaviour(gen_server).

%% API
-export ([	start_link/0,
			send/2,
			register_connection/3,
			unregister_connection/2, 
			all_connections/0,
			get_local_address_port/0,
			set_local_address/2,
			all_pids/0, local_pid/0,
			send_to_all/1,
			find_node/2,
			all_nodes/0, my_ip/0,
			all/0, all/1
		]).
		
-export ([	testing_register_connection/5,
			testing_unregister_connection/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Macros
-define(SERVER, ?MODULE).
-record (state, {node}).

%%====================================================================
%% API
%%====================================================================

% Send pass the Message to the node at Address and Port
send({Address, Port}, Message) ->
	io:format("Looking to send message to: ~p:~p~n", [Address, Port]),
	case ?DB:find_node(Address, Port) of
		[{{Address, Port}, Node}] -> 
			io:format("Node at ~p:~p known at ~p~n", [Address, Port, Node]),
			Pid = Node#node.pid, Address = Node#node.address,
			converse_connection:send(Address, Pid, Message), ok;
		[] ->			
			gen_server:call(?MODULE, {send, Address, Port, Message}, ?TIMEOUT)
	end.

send_to_all(Message) ->
	Pids = all_pids(),
	[X ! Message || X <- Pids].

all_connections() ->
	handle_get_all_connections().

register_connection(Address, Port, Info) ->
	MyIp = my_ip(),
	case converse_connection:open_new_connection_to(Address, Port, MyIp, ?DEFAULT_PORT) of
		{local_connection, LocalIp, LocalPort, _LocalSocket} ->
			gen_server:call(?SERVER, {set_local_address, LocalIp, LocalPort}, ?TIMEOUT);
		{connection, Pid, Socket} ->
			gen_server:call(?SERVER, {register_connection, Address, Port, Pid, Socket, Info}, ?TIMEOUT);
		Else ->
			io:format("Error when registering: ~p~n", [Else]),
			{error}
	end.
	
testing_register_connection(Address, Port, Pid, Socket, Info) ->
	gen_server:call(?SERVER, {register_connection, Address, Port, Pid, Socket, Info}, ?TIMEOUT).

unregister_connection(Address, Port) ->
	gen_server:call(?SERVER, {unregister_connection, Address, Port}, ?TIMEOUT).

testing_unregister_connection(Address, Port) ->
	gen_server:call(?SERVER, {unregister_connection, Address, Port}, ?TIMEOUT).

get_local_address_port() ->
	case ?DB:lookup(local_node) of
		[{local_node, Node}] ->
			{Node#node.address, Node#node.port};
		[] ->
			undefined
	end.
	
set_local_address(Ip, Port) ->
	gen_server:call(?SERVER, {set_local_address, Ip, Port}, ?TIMEOUT).

all() ->
	all(undefined).

all(Type) ->
	case Type of
		nodes -> all_nodes();
		pids -> all_pids();
		ips -> all_ips();
		undefined -> all_nodes()
	end.

all_nodes() ->
	?DB:select_all(node).

all_pids() ->
	AllNodes = all_nodes(),
	[X#node.pid || X <-AllNodes].
	
all_ips() ->
	AllNodes = all_nodes(),
	[X#node.address || X <-AllNodes].

find_node(Address, Port) ->
	% io:format("DB:find_node(~p, ~p) = ~p~n", [Address, Port, ?DB:find_node(Address, Port)]),
	case ?DB:find_node(Address, Port) of
		{{Address, Port}, Node} ->
			{remote_node, Node};
		[] ->
			local_node()
	end.

local_node() ->
	case ?DB:find_by_key({local_node}) of
		[{local_node}, Node] ->
			{local_node, Node};
		_Found ->
			not_found
	end.
	
local_pid() ->
	LocalNode = local_node(),
	LocalNode#node.pid.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->	
	process_flag(trap_exit, true),
	% Start the database
	converse_db:start(),
	% Start the local connection to self()
	pg2:start_link(), 
	pg2:create(?SERVER),
	Row = #node{key={local_node}, port=?DEFAULT_PORT, address=undefined},
	converse_db:insert(Row),
	{ok, ok}.

handle_call({send, Address, Port, Message}, _From, State) ->
	handle_forward_message(Address, Port, Message, State);

handle_call({register_connection, Address, Port, Pid, Socket, Tuple}, _From, State) ->
	handle_register_connection(Address, Port, Pid, Socket, Tuple, State);

handle_call({unregister_connection, Address, Port}, _From, State) ->
	handle_unregister_connection(Address, Port, State);

handle_call({set_local_address, Ip, Port}, _From, State) ->
	handle_set_local_address(Ip, Port, State);
	
handle_call(Request, _From, State) ->
	io:format("Received request ~p in converse_router~n", [Request]),
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
handle_info({'EXIT', _Pid, normal}, State) ->
	{noreply, State};

handle_info({'EXIT', Pid, Abnormal}, State) ->
	io:format("Received EXIT: ~p from ~p~n", [Abnormal, Pid]),
	% timer:sleep(?TIMEOUT),
	CurrNode = State#state.node,
	pg2:join(?SERVER, CurrNode#node.pid),
	NewState = #state{node=CurrNode},
	io:format("Finished init([~p])~n", [NewState]),	
	{noreply, NewState};
	
handle_info(Info, State) ->
	io:format("Info: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	pg2:leave(?SERVER, self()),
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
	io:format("In handle_register_connection: ~p:~p~n", [Address, Port]),
	case find_node(Address, Port) of
		{remote_node, _Node} ->
			io:format("Already registered ~p~n", [Address]),
			{reply, dups, State};
		{local_node, _Node} ->
			io:format("Found local_node~n"),
			{reply, local_node, State};
		[] ->			
			?DB:insert_node(Address, Port, Pid, Socket, Tuple),
			pg2:join(?SERVER, Pid),
			{reply, ok, State}
	end.
	
handle_unregister_connection(Address, Port, State) ->
	Reply = case find_node(Address, Port) of
		{_, Node} ->
			Pid = Node#node.pid,
			?DB:delete_node(Address, Port),
			pg2:leave(?SERVER, Pid),
			ok;
		_ ->
			ok
	end,
	{reply, Reply, State}.

handle_forward_message(Address, Port, Message, State) ->
	case find_node(Address, Port) of
		{local_node, Node} ->
			io:format("Sending ~p to local node~n", [Message]),
			converse_connection:send(Node, Message);
		{remote_node, _Node} -> 
			handle_remote_forward_message(Address, Port, Message, State);
		unknown_node ->
			handle_remote_forward_message(Address, Port, Message, State)
	end.
	
handle_remote_forward_message(Address, Port, Message, State) ->
	{CurrAddr,CurrPort} = get_local_address_port(),
	case converse_connection:open_new_connection_to(Address, Port, CurrAddr, CurrPort) of
		{local_connection, LocalIp, LocalPort, LocalSocket} ->
			CustNode = #node{key={local_node}, address=LocalIp, port=LocalPort, socket=LocalSocket},
			converse_connection:send(CustNode, Message),
			converse_db:insert(CustNode),
			{reply, ok, State};
		fail ->
			{reply, ok, State};
		{connection, _Pid, Socket} ->
			CustNode = #node{address=Address, port=Port, socket=Socket},
			converse_connection:send(CustNode, Message),
			register_connection(Address, Port, {}),
			{reply, ok, State}				
	end.

handle_get_all_connections() ->
	?DB:select_all(node).
	
handle_set_local_address(Address, Port, State) ->
	Row = #node{key={local_node}, port=Port, address=Address},
	converse_db:insert(Row),
	{reply, ok, State}.

% handle_init_and_start_acceptor(State) ->	
% 	LocalIP = my_ip(), LocalPort = ?DEFAULT_PORT,
% 	io:format("in handle_init_and_start_acceptor with ~p~n", [LocalIP]),
% 	NewState = case converse_listener:start_acceptor(LocalPort, LocalIP, self()) of
% 		{connection, Pid, Socket} -> 			
% 			new_state_node(LocalIP, LocalPort, Pid, Socket, {});
% 		_Else ->
% 			CurrNode = State#state.node,
% 			pg2:join(?SERVER, CurrNode#node.pid),
% 			State
% 	end,
% 	io:format("Finished init([~p])~n", [NewState]),
% 	NewState.

my_ip() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, HostEntry} = inet:gethostbyname(Hostname),
    erlang:hd(HostEntry#hostent.h_addr_list).

% new_state_node(Address, Port, Pid, Socket, Tuple) ->
% 	BaseNode = #node{key={Address, Port}},
% 	NewNode = BaseNode#node{address=Address,port=Port,pid=Pid,socket=Socket,tuple=Tuple},
% 	#state{node=NewNode}.
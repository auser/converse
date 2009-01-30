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
			register_connection/3,
			unregister_connection/2, 
			get_all_connections/0,
			get_local_address_port/0,
			set_local_address/2
		]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% Macros
-define(SERVER, ?MODULE).
-record (state, {node}).

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
	io:format("Looking to send message to: ~p:~p~n", [Address, Port]),
	case ?DB:find_node(Address, Port) of
		[{{Address, Port}, Node}] -> 
			io:format("Node at ~p:~p known at ~p~n", [Address, Port, Node]),
			Pid = Node#node.pid, Address = Node#node.address,
			talker_connection:send(Address, Pid, Message), ok;
		[] ->			
			gen_server:call(?MODULE, {send, Address, Port, Message}, ?TIMEOUT)
	end.

get_all_connections() ->
	handle_get_all_connections().

register_connection(Address, Port, Info) ->
	% Pid, Socket
	MyIp = my_ip(),
	case talker_connection:open_new_connection_to(Address, Port, MyIp, ?DEFAULT_PORT) of
		{connection, Pid, Socket} ->
			gen_server:call(?SERVER, {register_connection, Address, Port, Pid, Socket, Info}, ?TIMEOUT);
		Else ->
			io:format("Error when registering: ~p~n", [Else]),
			{error}
	end.	

unregister_connection(Address, Port) ->
	gen_server:call(?SERVER, {unregister_connection, Address, Port}, ?TIMEOUT).

get_local_address_port() ->
	gen_server:call(?SERVER, {get_local_address_port}, ?TIMEOUT).
	
set_local_address(Ip, Port) ->
	gen_server:call(?SERVER, {set_local_address, Ip, Port}, ?TIMEOUT).
	
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
	% Start the database
	talker_db:start(),
	% Start the local connection to self()
	LocalIP = my_ip(), LocalPort = ?DEFAULT_PORT,
	io:format("Starting acceptor at ~p:~p~n", [LocalIP, LocalPort]),
	case talker_acceptor:start_acceptor(LocalPort, LocalIP) of
		{connection, Pid, Socket} ->
			NewState = update_state_node(LocalIP, LocalPort, Pid, Socket, {}),
			io:format("Started router with: ~p~n", [NewState]);
		_Else ->
			NewState = #state{}
	end,
	io:format("Finished init([]) with ~p~n", [NewState]),
	{ok, NewState}.

handle_call({send, Address, Port, Message}, _From, State) ->
	handle_forward_message(Address, Port, Message, State);
	
handle_call({register_connection, Address, Port, Pid, Socket, Tuple}, _From, State) ->
	handle_register_connection(Address, Port, Pid, Socket, Tuple, State);

handle_call({unregister_connection, Address, Port}, _From, State) ->
	handle_unregister_connection(Address, Port, State);

handle_call({get_local_address_port}, _From, State)	->
	handle_get_local_address_port(State);

handle_call({set_local_address, Ip, Port}, _From, State) ->
	handle_set_local_address(Ip, Port, State);
	
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
handle_info({'EXIT', _Pid, normal}, State) ->
	{noreply, State};

handle_info({'EXIT', Pid, _Abnormal}, State) ->
	timer:sleep(?TIMEOUT),
	Ip = my_ip(), Port = ?DEFAULT_PORT,
	case talker_acceptor:start_acceptor(Port, Ip, self()) of
		{connection, Pid, Socket} ->
			NewState = #node{address = Ip, port = Port, pid = Pid, socket = Socket},
		    {noreply, NewState};
		_Else ->
			{noreply, State}
	end;	
	
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
	case ?DB:find_node(Address, Port) of
		[{{Address, Port}, _}] ->
			io:format("Already registered ~p~n", [Address]),
			{reply, dups, State};
		[] ->			
			{MyAddress, MyPort} = {my_ip(), ?DEFAULT_PORT},
			case Address =:= MyAddress andalso Port =:= MyPort of
				true ->
					{reply, self, State};
				_ ->
					?DB:insert_node(Address, Port, Pid, Socket, Tuple),
					{reply, ok, State}
			end
	end.
	
handle_unregister_connection(Address, Port, State) ->
	?DB:delete_node(Address, Port),
	{reply, ok, State}.

handle_forward_message(Address, Port, Message, State) ->
	{MyAddress, MyPort} = get_local_address_port(),
	io:format("~p =:= ~p andalso ~p =:= ~p~n", [Address, MyAddress, Port, MyPort]),
	case (Address =:= MyAddress andalso Port =:= MyPort) of
		true -> self() ! Message;
		_ ->
			handle_remote_forward_message(Address, Port, Message, State)
	end.
	
handle_remote_forward_message(Address, Port, Message, State) ->
	case ?DB:find_node(Address, Port) of
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
	?DB:select_all(node).

handle_get_local_address_port(State) ->
	LocalAddress = State#node.address,
	LocalPort = State#node.port,
	{reply, {LocalAddress, LocalPort}, State}.

handle_set_local_address(Ip, Port, State) ->
	NewState = State#node{address = Ip, port = Port},
	{ok, NewState}.

my_ip() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, HostEntry} = inet:gethostbyname(Hostname),
    erlang:hd(HostEntry#hostent.h_addr_list).

update_state_node(Address, Port, Pid, Socket, Tuple) ->
	NewNode = #node{address=Address,port=Port,pid=Pid,socket=Socket,tuple=Tuple},
	#state{node=NewNode}.
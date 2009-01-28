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
			unregister_connection/2, 
			get_all_connections/0,
			set_local_address/2,
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

send({Address, Port, Pid}, Message) ->
	case ets:lookup(?MODULE, {Address, Port}) of
		[{{Address, Port}, {_LPid, Socket}}] ->
			talker_connection:send({Address, Port, Socket}, Pid, Message),
			ok;
		[] ->
			gen_server:call(?MODULE, {send, Address, Port, Pid, Message}, ?TIMEOUT)
	end.

get_all_connections() ->
	handle_get_all_connections().

register_connection(Address, Port, Pid, Socket) ->
	gen_server:call(?SERVER, {register_connection, Address, Port, Pid, Socket}, ?TIMEOUT).

unregister_connection(Address, Port) ->
	gen_server:call(?SERVER, {unregister_connection, Address, Port}, ?TIMEOUT).

set_local_address(Address, Port) ->
	gen_server:call(?SERVER, {set_local_address, Address, Port}, ?TIMEOUT).

get_local_address_port() ->
	NewPort = ets:lookup(?SERVER, {local_address_port}),
	io:format("Lookup: ~p~n", [NewPort]),
    case ets:lookup(?SERVER, local_address_port) of
     	[{local_address_port, Value}] ->
 	    Value;
 	[] ->
 	    undefined
    end.

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
	ets:new(?MODULE, [set, protected, named_table]),
    {ok, ok}.

handle_call({send, Address, Port, Pid, Message}, _From, State) ->
	handle_forward_message(Address, Port, Pid, Message, State);
	
handle_call({register_connection, Address, Port, Pid, Socket}, _From, State) ->
	handle_register_connection(Address, Port, Pid, Socket, State);

handle_call({unregister_connection, Address, Port}, _From, State) ->
	handle_unregister_connection(Address, Port, State);
	
handle_call({set_local_address, Address, Port}, _From, State) ->
	handle_set_local_address(Address, Port, State);
	
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
handle_register_connection(Address, Port, Pid, Socket, State) ->
	case ets:lookup(?MODULE, {Address, Port}) of
		[{{Address, Port}, _}] ->
			{reply, dups, State};
		[] ->
			ets:insert(?MODULE, {{Address, Port}, {Pid, Socket}}),
			{reply, ok, State}
	end.
	
handle_unregister_connection(Address, Port, State) ->
	ets:delete(?MODULE, {Address, Port}),
	{reply, ok, State}.

handle_set_local_address(Address, Port, State) ->
	ets:insert(?MODULE, {local_address_port, {Address, Port}}),
	{reply, ok, State}.

handle_forward_message(Address, Port, Pid, Message, State) ->
	case ets:lookup(?MODULE, {Address, Port}) of
		[{{Address, Port}, {_LPid, Socket}}] ->
			talker_connection:send({Address, Port, Socket}, Pid, Message),
			{reply, ok, State};
		[] ->
			{InitAddr, InitPort} = get_local_address_port(),
			case talker_connection:open_new(Address, Port, InitAddr, InitPort) of
				{local_ip, NewIP, NewPort, NewPid, NewSocket} ->
					talker_connection:send({Address, Port, NewSocket}, Pid, Message),
					ets:insert(?MODULE, {local_address_port, {NewIP, NewPort}}),
					ets:insert(?MODULE, {{Address, Port}, {NewPid, NewSocket}}),
					{reply, ok, State};
				fail ->
					{reply, ok, State};
				{connection, LocalPid, NewSocket} ->
					talker_connection:send({Address, Port, NewSocket}, Pid, Message),
					ets:insert(?MODULE, {{Address, Port}, {LocalPid, NewSocket}}),
					{reply, ok, State}
			end
		end.

handle_get_all_connections() ->
	Add = ets:tab2list(?MODULE),
	io:format("Addresses ~p~n", [Add]),
	Add.
%%%%%
% Routes ips to pid files
%%%%%

-module (talk_router).

-behaviour(gen_server).

%% API
-export([
		start_link/0, 
		send_message/2, 
		remove_connection/2, 
		add_connection/4, 
		set_my_address/2, 
		get_my_address/0]).

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
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% Send a message to the node at the address given.
% If the node is known about through us, then send the message directly through 
% this node
% otherwise, send the message to ourselves with a timestamp to allow the message to continue
% along down the chain
send_message({Add, Port, Pid}, Message) ->
	case ets:lookup(?MODULE, {Add, Port}) of
		[{{Add, Port}, {_Pid, Socket}}] ->
			talk_to:send_message({Add, Port, Socket}, Pid, Message),
			ok;
		[] ->
			gen_server:call(?SERVER, {send_message, Add, Port, Pid, Message}, 25000)
	end.

remove_connection(Add, Port) ->
	gen_server:call(?SERVER, {remove_connection, Add, Port}, 25000).

add_connection(Add, Port, Pid, Socket) ->
	gen_server:call(?SERVER, {add_connection, Add, Port, Pid, Socket}, 25000).

set_my_address(Add, Port) ->
	gen_server:call(?SERVER, {set_my_address, Add, Port}, 25000).

get_my_address() ->
	case ets:lookup(?MODULE, local_address) of
		[{local_address, Port}] ->
			Port;
		[] ->
			undefined
	end.



%%====================================================================
%% gen_server callbacks
%%====================================================================

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	ets:new(?MODULE, [set, protected, named_table]),
    {ok, ok}.

% handle_call is invoked in response to gen_server:call

handle_call({send_message, Add, Port, Pid, Message}, _F, State) ->
	case ets:lookup(?MODULE, {Add, Port}) of
		[{{Add, Port}, {_Pid, Socket}}] ->
			talk_to:send_message({Add, Port, Socket}, Pid, Message),
			{reply, ok, State};
		[] ->
			{SenderAddress, SenderPort} = get_my_address(),
			case talk_to:open_new(Add, Port, SenderAddress, SenderPort) of
				{local_address, MyAdd, MyPort, MyPid, MySocket} ->
					talk_to:send_message({Add, Port, MySocket}, Pid, Message),
					set_my_address(MyAdd, MyPort),
					add_connection(Add, Port, Pid, Socket),
					{reply, ok, State};
				{connection, LPid, NewSock} ->
					talk_to:send_message({Add, Port, NewSocket}, Pid, Message),
					add_connection(Add, Port, LPid, Message),
					{reply, ok, State};
				error ->
					{reply, ok, State}
			end
	end;

% Add the connection to the ets table
handle_call({add_connection, Add, Port, Pid, Socket}, _From, State) ->
	case ets:lookup(?MODULE, {Add, Port}) of
		[{{Add, Port}, _}] ->
			{reply, already_exists, State};
		[] ->
			ets:insert(?MODULE, {{Add, Port}, {Pid, Socket}}),
			{reply, ok, State}
	end;

handle_call({remove_connection, Add, Port}, _From, State) ->
	ets:delete(?MODULE, {Add, Port}),
	{reply, ok, State};
	
handle_call({set_my_address, Add, Port}, _From, State) ->
	ets:insert(?MODULE, {local_address, {Add, Port}}),
	{reply, ok, State};

handle_call(_Request, _From, State) ->
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
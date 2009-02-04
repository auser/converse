-module (converse_tcp).
-include("converse.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/1, accept_loop/4, stop/1]).
-export ([send/2, set_receive_function/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define (SERVER, ?MODULE).

-record(tcp_server, {max=100, connections=0, acceptor, listen, receiver, config}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link(ReceiverFunction) ->
  start_link(?DEFAULT_CONFIG, ReceiverFunction).

start_link(ReceiverFunction,Config) ->
	?TRACE("Starting with", [ReceiverFunction, Config]),
	{ok, Pid} = gen_server:start_link(?MODULE, [Config, ReceiverFunction], []),
	Pid.

stop(Pid) ->
	gen_server:cast(Pid, {stop}).

set_receive_function(Pid, Fun) ->
	gen_server:call(Pid, {set_receive_function, Fun}, ?TIMEOUT).
	
%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end 
%%--------------------------------------------------------------------
init([Config, ReceiverFunction]) ->
  process_flag(trap_exit, true),
	?TRACE("Calling server(Config, ReceiverFunction)", [Config]),
  server(Config, ReceiverFunction, #tcp_server{config=Config}).

%%--------------------------------------------------------------------
%% @spec 
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end 
%%--------------------------------------------------------------------
handle_call({set_receive_function, Fun}, _From, State) ->	
	Pid = proc_lib:spawn_link(Fun),
	NewState = State#tcp_server{receiver = Pid},
	{reply, {ok}, NewState};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end 
%%--------------------------------------------------------------------
handle_cast({server_accepted, Pid}, State = #tcp_server{acceptor=Pid,connections=Conn}) ->
	ConnCount = Conn+1,
	?TRACE("Server accepted pid", [ConnCount, Pid]),
	{noreply, listen_loop(State#tcp_server{connections=ConnCount})};

handle_cast(stop, State) ->
	{noreply, normal, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, normal}, State=#tcp_server{acceptor=Pid}) ->
	{noreply, listen_loop(State)};
  
handle_info({'EXIT', Pid, Reason}, State=#tcp_server{receiver=Pid, connections=Conn}) ->
	case Reason of
	  normal -> 
			ok;
	  _ ->
			?TRACE("error with connection", [Reason])
	end,
	NewConnCount = Conn-1,
	NewState = State#tcp_server{ connections = NewConnCount },
	{noreply, case Pid of
	    undefined -> 
			listen_loop(NewState);
	    _ -> 
			NewState
	  end};
    
handle_info(Info, State) ->
  ?TRACE("Received Info", [Info]),
  {noreply, State}.

terminate(_Reason, #tcp_server{ listen = Listen }) ->
    gen_tcp:close(Listen).

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end 
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

% Listening methods
server(Config, ReceiverFunction, State) ->
	Port = utils:safe_integer(config:parse(port, Config)),	
	NewConfig = config:update(port, Port, Config),
	?TRACE("Starting server on port", [Port, NewConfig]),
	Receiver = case config:parse(receiver, NewConfig) of
		{} -> undefined;
		Rec -> Rec
	end,
	?TRACE("Starting server with receiver", [Receiver]),
	case gen_tcp:listen(Port, ?PACKET_SETUP) of
		{ok, Socket} ->			
			RPid = case Receiver of
				undefined -> 					
					[M,F] = ReceiverFunction, A = [self()],
					spawn(M,F,A);
				_ -> Receiver
			end,
			ANewConfig = config:update(receiver, RPid, NewConfig),
			{ok, listen_loop(State#tcp_server{config=ANewConfig, listen=Socket, receiver=RPid})};
		{error, Reason} ->
			{stop, Reason}
	end.
  
listen_loop(State = #tcp_server{ max = Max, connections = Connections}) when Max == Connections ->
	?TRACE("Connection limit reached", [Max]),
	State#tcp_server{ acceptor = undefined };

listen_loop(State = #tcp_server{ listen = Listen, receiver = Receiver}) ->
  Pid = proc_lib:spawn_link(?MODULE, accept_loop, [self(), Listen, Receiver, State]),
	?TRACE("Listen loop",[self(), Listen, Receiver, State]),
  State#tcp_server{acceptor=Pid, receiver = Receiver}.
  
accept_loop(Server, Listen, Receiver, _State) ->	
	?TRACE("Socket is open", [Listen]),
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			gen_server:cast(Server, {server_accepted, self() }),
			handler_loop( Socket, Server, Receiver, erlang:make_ref() );
		{error, closed} ->
			?TRACE("Closed tcp socket", []),
			exit({error, closed});
		Other ->
			?TRACE("Accept failed", [Other])
	end.
  
handler_loop(Socket, Server, Receiver, Ref) ->
	case read_socket(Socket) of
		{deliver, Data} ->
			Receiver ! Data;
		_Other ->
			ok
	end,
	handler_loop(Socket, Server, Receiver, Ref).

read_socket(Socket) ->
	case gen_tcp:recv(Socket, 0) of
	    {ok, Packet} ->
					converse_packet:decode(Packet);
			{error, timeout} ->
					normal;
	    {error, closed} ->
					normal
	end.
	
% Loquacious methods
send(Node, Data) ->
	case Node of
		{_Address, _Port} ->
			send_to_address_and_port(Node, Data);
		Name ->
			send_to_node(Name, Data)
	end.
	
send_to_node(Nodename, Data) ->
	case get_node_address(Nodename) of
		{error, _Error} ->
			ok;
		Add ->
			?TRACE("Sending ~p to ~p~n", [Data, Add]),
			send({Add, ?DEFAULT_PORT}, Data)
	end.

get_node_address(Nodename) ->
	case net_adm:ping(Nodename) of
		pang ->
			error;
		pong ->
			case net_kernel:node_info(Nodename) of
				{ok, NodeInformation} ->
					[Add|_] = [ element(1, element(2, element(2,T))) || T <- NodeInformation, element(1, T) =:= address],
					Add;
				{error, Error} ->
					?TRACE("Error in get_node_address", [Error]),
					{error, Error}
			end
	end.
	
send_to_address_and_port({Address, Port}, Data) ->
	case new_connection(Address, Port) of
		{ok, Socket} ->
			Bin = converse_packet:encode({deliver, Data}),
			case gen_tcp:send(Socket, Bin) of
				{error, timeout} ->
					gen_tcp:close(Socket);
				ok ->
					ok;
				{error, closed} ->
					gen_tcp:close(Socket);
				{error, _Reason} ->
					gen_tcp:close(Socket)
			end,
			ok;
		{error, Reason} ->
			?TRACE("Error when trying to send data", [Reason])
	end.
	
new_connection(Address, Port) ->
	new_connection_with_retry(Address, Port, ?TIMES_TO_RETRY).

new_connection_with_retry(_Address, _Port, 0) ->
	{error, closed};

new_connection_with_retry(Address, Port, RetriesLeft) ->
    case gen_tcp:connect(Address, Port, [binary, {active, false}], ?TIMEOUT) of
        {ok, Socket} ->
					{ok, Socket};
				{error, Reason} ->
				  ?TRACE("reconnect to socket", [Address, Reason]),
					MoreRetries = RetriesLeft - 1,
					timer:sleep(100),
		      case new_connection_with_retry(Address, Port, MoreRetries) of
							{error, closed} ->
								?TRACE("Error creating connection to ~p:~p~n", [Address, Port]),
								{error, closed};
						Socket ->
							{ok, Socket}
			end
    end.

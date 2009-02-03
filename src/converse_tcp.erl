-module (converse_tcp).
-include("converse.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/1, accept_loop/4]).
-export ([send/2, set_receive_function/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define (SERVER, ?MODULE).

-record(tcp_server, {max=100, connections=0, acceptor, listen, receiver}).

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

start_link(Config, ReceiverFunction) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [Config, ReceiverFunction], []).

set_receive_function(Fun) ->
	gen_server:call(?MODULE, {set_receive_function, Fun}, ?TIMEOUT).
	
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
  server(Config, ReceiverFunction, #tcp_server{}).

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
	Pid = spawn_link(Fun),
	?TRACE("Setting receive function as", [Fun, Pid]),
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
	{noreply, listen_loop(State#tcp_server{connections=ConnCount})};

handle_cast(stop, State) ->
	{stop, normal, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info({'EXIT', Pid, normal}, State=#tcp_server{acceptor=Pid}) ->
	{noreply, listen_loop(State)};
    
handle_info({'EXIT', Pid, Reason}, State=#tcp_server{acceptor=Pid}) ->
	?TRACE("tcp_listener_error", [Reason]),
	timer:sleep(100),
	{noreply, listen_loop(State)};
  
handle_info({'EXIT', _Pid, Reason}, State=#tcp_server{acceptor=Pid, connections=Conn}) ->
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
	Port = Config#config.port, Receiver = Config#config.receiver,
	case gen_tcp:listen(Port, ?PACKET_SETUP) of
		{ok, Socket} ->			
			RPid = case Receiver of
				undefined -> 
					spawn(ReceiverFunction);
				_ -> 
					Receiver
			end,
			?TRACE("Server setup and starting listen_loop", [Socket, RPid]),
			{ok, listen_loop(State#tcp_server{listen=Socket, receiver=RPid})};
		{error, Reason} ->
			{stop, Reason}
	end.
  
listen_loop(State = #tcp_server{ max = Max, connections = Connections}) when Max == Connections ->
	?TRACE("Connection limit reached", [Max]),
	State#tcp_server{ acceptor = undefined };

listen_loop(State = #tcp_server{ listen = Listen, receiver = Receiver}) ->
	?TRACE("Spawned rpid", [Receiver]),
  Pid = proc_lib:spawn_link(?MODULE, accept_loop, [self(), Listen, Receiver, State]),
  State#tcp_server{acceptor=Pid, receiver = Receiver}.
  
accept_loop(_Server, Listen, Receiver, _State) ->
	case gen_tcp:accept(Listen) of
		{ok, Socket} ->
			gen_server:cast(?SERVER, {server_accepted, self() }),
			handler_loop( Socket, Listen, Receiver, erlang:make_ref() );
		{error, closed} ->
			?TRACE("Closed tcp socket", []),
			exit({error, closed});
		Other ->
			?TRACE("Accept failed", [Other]),
			exit({error, accept_failed})
	end.
  
handler_loop(Socket, Server, Receiver, Ref) ->
	{deliver, Data} = read_socket(Socket),	
	Receiver ! Data,
	?TRACE("handler_loop received", [Data, Receiver]),
	handler_loop(Socket, Server, Receiver, Ref).

read_socket(Socket) ->
	case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
	    {ok, Packet} ->
					?TRACE("Reading packet", [Packet]),
					converse_packet:decode(Packet);
			{error, timeout} ->
					{normal};
	    {error, closed} ->
					?TRACE("Error", [])
	end.
	
% Loquacious methods
send({Address, Port}, Data) ->
	case new_connection(Address, Port) of
		{ok, Socket} ->
			Bin = converse_packet:encode({deliver, Data}),
			case gen_tcp:send(Socket, Bin) of
				ok ->
					?TRACE("Data sent", [Bin]), ok;
				{error, closed} ->
					gen_tcp:close(Socket);
				{error, Reason} ->
					?TRACE("Error sending data", [Reason]), 
					gen_tcp:close(Socket)
			end,
			ok;
		{error, Reason} ->
			?TRACE("Error when trying to send data", [Reason])
	end.
	
new_connection(Address, Port) ->
    case gen_tcp:connect(Address, Port, [binary, {active, false}], ?TIMEOUT) of
        {ok, Socket} ->
					?TRACE("Ready for connection", [Socket]),
					{ok, Socket};					
        {error, Reason} ->
					?TRACE("couldn't connect", [Address, Port, Reason]),
					{error, Reason}
    end.

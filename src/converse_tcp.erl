-module (converse_tcp).

-include ("converse.hrl").
-include ("tcp.hrl").

-behaviour (gen_server).

-record (state, {
                  port,             % port to start server on
                  accept_pid,       % Accepting PID
                  successor,        % Layer's acceptor
                  config,           % Configuration for server
                  connections       % Current connections
                }).

-record (server, {
                  successor,        % successor
                  socket,           % socket process
                  receiver          % receiving process
                }).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         
-export ([cast_message/2, send_message/2, layers_receive/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
cast_message(Addr, Msg) -> 
  gen_server:cast(?SERVER, {send, Addr, Msg}).

send_message(Addr, Msg) ->
  gen_server:call(?SERVER, {send, Addr, Msg}).

layers_receive(Msg) ->
  case Msg of
    {data, Server, Data} ->
      io:format("~p received in layers_receive: ~p~n", [?MODULE, Data]),
      Server#server.receiver ! {reply, Server, Data},
      io:fwrite("response: ~p~n", [Data]);
    Else ->
      io:format("~p received unknown message: ~p~n", [?MODULE, Else]),
      Else
  end.
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

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
init([Config]) ->
  process_flag(trap_exit, true),
  % Get server options
  [Port,Opts,Successor] = config:fetch_or_default_config([port,sock_opts,successor], Config, ?DEFAULT_CONFIG),
  % Function to run on reception of a packet
  Self = self(),
  Server = #server{successor = Successor, receiver = Self},
  Fun = fun(Socket) -> parse_packet(Socket, Server) end,
  
  tcp_server:stop(Port),
  
  {ok, AcceptPid} = tcp_server:start_raw_server(Port, Fun, 10240,0),  
  
  {ok, #state{
    port = Port,
    successor = Successor,
    accept_pid = AcceptPid,
    config = Config,
    connections = 0
  }}.
  
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
% Need to handle the case that the other server does not respond
handle_call({send, Addr, Msg}, From, #state{port = Port} = State) ->
  {ok, Sock} = gen_tcp:connect(Addr, Port, [binary, {packet, raw}, {active, true}]),
  DataToSend = converse_socket:encode(Msg),
  Rep = gen_tcp:send(Sock, DataToSend),
  {reply, Rep, State};
  
handle_call(Request, _From, State) ->
  io:format("Received unknown request ~p~n", [Request]),
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({send, Addr, Msg}, #state{port = Port} = State) ->
  {ok, Sock} = gen_tcp:connect(Addr, Port, [binary, {packet, raw}, {active, true}]),
  DataToSend = converse_socket:encode(Msg),
  gen_tcp:send(Sock, DataToSend),
  {noreply, State};
handle_cast(stop, State) ->
  {stop, normal, State};
  
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'EXIT', _Pid, _Reason}, State) -> {noreply, State};
handle_info({reply, Server, Reply}, State) ->
  io:format("Received reply and TCP sending it off: ~p~n", [Reply]),
  ?TCP_SEND(Server#server.socket, Reply),
  {noreply, State};
handle_info(Info, State) ->
  io:format("Received unknown info: ~p~n", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{port = Port} = State) ->
  tcp_server:stop(Port),
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
parse_packet(Socket, Server) ->
  receive
    {tcp, Socket, Bin} ->
      % NewServer = Server#server{socket = Socket},
      DataToSend = {data, Socket, converse_socket:decode(Bin)},
      case Server#server.successor of
        undefined -> layers_receive(DataToSend);
        Suc -> layers:pass(Suc, DataToSend)
      end,
      parse_packet(Socket, Server);
    {tcp_closed, Socket} ->
      io:format("Socket closing~n"),
      ok
  end.
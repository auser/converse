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
                  receiver          % receiving process
                }).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
         
-export ([send_message/1, layers_receive/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
send_message(Msg) -> 
  gen_server:call(?SERVER, Msg).

layers_receive(Msg) ->
  case Msg of
    {data, From, Data} ->
      io:format("~p received in layers_receive: ~p~n", [?MODULE, Data]),
      Data;
    Else ->
      io:format("~p received unknown message: ~p~n", [?MODULE, Else]),
      Else
  end.
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Config) ->
  case gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []) of
    {ok, Pid} -> {ok, Pid};
    Other -> Other
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
init([Config]) ->
  process_flag(trap_exit, true),
  % Get server options
  [Port,Opts,Successor] = config:fetch_or_default_config([port,sock_opts,successor], Config, ?DEFAULT_CONFIG),
  % Function to run on reception of a packet
  Self = self(),
  Server = #server {successor = Successor, receiver = Self},
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
handle_call({send, Msg}, _From, #state{port = Port} = State) ->
  {ok, Sock} = tcp_server:start_client({0,0,0,0}, Port, 10),
  gen_tcp:send(Sock, converse_socket:encode(Msg)),
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
handle_info(_Info, State) ->
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
      DataToSend = {data, Server#server.receiver, converse_socket:decode(Bin)},
      Reply = case ?debug of
        true -> layers_receive(DataToSend);
        false -> layers:pass(Server#server.successor, DataToSend)
      end,
      io:format("Received ~p~n", [Reply]),
      ok = ?TCP_SEND(Socket, Reply),
      parse_packet(Socket, Server);
    {tcp_closed, Socket} ->
      io:format("Socket closing~n"),
      ok;
    {packet, Packet} ->
      ok = ?TCP_SEND(Socket, Packet),
      parse_packet(Socket, Server)
  end.
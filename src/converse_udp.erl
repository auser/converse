%%%-------------------------------------------------------------------
%%% File    : converse_udp.erl
%%% Author  : Ari Lerner
%%% Description : 
%%%
%%% Created :  Mon Mar  2 11:59:29 PST 2009
%%%-------------------------------------------------------------------

-module (converse_udp).
-behaviour(gen_server).
-include ("converse.hrl").

%% API
-export([start_link/1, start_named/2]).
-export ([send/3,send_to_named/4, send_message/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(udp_state, {
          config,         % Global config
          successor,      % Successor
          socket,         % udp socket receiver
          debug           % should we show our true colors (debugging)
        }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_named(ServerName, Config) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [ServerName, Config], []).
  
start_link(Config) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [?MODULE, Config], []).

send(Message, IP, Port) -> 
  Name = converse_utils:registered_name(converse, integer_to_list(Port)),
  send_to_named(Name, Message, IP, Port).
send_to_named(Named, Message, IP, Port) -> 
  gen_server:call(Named, {send_message, Message, IP, Port}).
send_to_socket(Socket, IP, Port, Message) -> gen_server:cast({send_to_socket, Socket, IP, Port, Message}).

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
init([Server, Config]) ->
  process_flag(trap_exit, true),
  [Port] = config:fetch_or_default_config([port], Config, ?DEFAULT_CONFIG),
  
  case open_udp(Port) of
    error ->
        {stop, {error, ?MODULE, ?LINE, Port, "cannot open port"}};
    Socket ->
      ServerName = converse_utils:registered_name(Server, "udp"),
      State = #udp_state {
        socket = Socket,
        config = Config,
        debug = ?debug
      },
      {ok, State}
  end.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({send_message, Message, IP, Port}, _From, #udp_state{socket = Socket} = State) ->
  send_message(Socket, IP, Port, Message),
  {reply, Socket, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({send_to_socket, Socket, IP, Port, Message}, State) ->
  gen_udp:send(Socket, IP, Port, converse_packet:pack(Message)),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({udp, Socket, IP, Port, Bin}, State) ->
    Term = try converse_packet:unpack(Bin) catch _:Why -> ?LOG_MESSAGE(Why),{erlang:list_to_atom(binary_to_list(Bin))} end,
    {noreply, handle_dispatch(State, Socket, IP, Port, Term)};

handle_info({'EXIT', _Pid, normal}, State) -> 
  {noreply, State};

handle_info({'EXIT', _Pid, Reason}, #udp_state{config = Config} = State) -> 
  ?DEBUG_LOG(true, "Abnormal exit: ~p~n", [Reason]),
  ?MODULE:start_link(Config),
  {noreply, State};

handle_info(Info, State) ->
  case State#udp_state.debug of
      true -> io:format("Recevied generic info: ~p",[Info]);
      _ -> ok
  end,
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

open_udp(Port) ->
  try
      {ok, Socket} = gen_udp:open(Port, [binary]), Socket
  catch
      _:Why -> ?DEBUG_LOG(true, "Error", [Why]), error
  end.

send_message(Socket, IP, Port, Message) ->
  gen_udp:send(Socket, IP, Port, converse_packet:pack(Message)).

% ping
handle_dispatch(State, Socket, IP, Port, {ping}) ->
  ?DEBUG_LOG(State#udp_state.debug, "Received ping~n", []),
  send_message(Socket, IP, Port, {pong}),
  State;

handle_dispatch(State, Socket, _IP, _Port, Req) ->
  ?DEBUG_LOG(State#udp_state.debug, "Received request: ~p~n", [Req]),
  handle_pass_to_successor(Socket, Req, State),
  State.
  
handle_pass_to_successor(Socket, Msg, State) ->
  Suc = State#udp_state.successor,
  case Suc of
    undefined ->       
      ?DEBUG_LOG(State#udp_state.debug, "Received message with no successor: ~p~n", [Msg]);
    Suc -> 
      spawn(fun() -> layers:pass(Suc, {received_packet, Socket, Msg}) end)
  end.
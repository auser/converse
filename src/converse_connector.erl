%%%-------------------------------------------------------------------
%%% File    : converse_connector.erl
%%% Author  : Ari Lerner <arilerner@mac.com>
%%% Description : Connector
%%%-------------------------------------------------------------------
-module (converse_connector).
-include ("converse.hrl").

-behaviour (gen_server).

-export ([send/2, start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
send({Address, Port}, Message) ->
	gen_server:call(?SERVER, {deliver, Address, Port, Message}, ?TIMEOUT).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
	{ok, ok}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({deliver, Address, Port, Message}, _From, State) ->
	handle_deliver_message({Address, Port}, Message),
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

handle_deliver_message({Address, Port}, Message) ->
	BinMess = term_to_binary({deliver, Message}),
	Socket = new_connection(Address, Port),
	case gen_tcp:send(Socket, BinMess) of
		{error, timeout} ->
			gen_tcp:close(Socket);
		ok ->
			ok;
		{error, closed} ->
			gen_tcp:close(Socket);
		{error, _Reason} ->
			gen_tcp:close(Socket)
	end.

new_connection(Address, Port) ->
    case gen_tcp:connect(Address, Port, ?PACKET_SETUP, ?TIMEOUT) of
        {ok, Socket} ->
			Socket;
		{error, Reason} ->
		    io:format("reconnect to ~p because socket is ~p~n", [Address, Reason]),
            case new_connection_retry(Address, Port, ?TIMES_TO_RETRY) of
				fail ->
					io:format("Error creating connection to ~p:~p~n", [Address, Port]),
					fail;
				Socket ->
					Socket
			end
    end.

new_connection_retry(_Address, _Port, 0) ->
	fail;
	
new_connection_retry(Address, Port, Num) ->
	case new_connection(Address, Port) of
		fail ->
			NewNum = Num - 1,
			new_connection_retry(Address, Port, NewNum);
		Socket ->
			Socket
	end.
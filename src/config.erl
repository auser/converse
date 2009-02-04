-module (config).
-include("converse.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, get_config/1, set_config/2, stop/1, get_key/2, parse/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record (config_state, {config}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end 
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link(?MODULE, ?DEFAULT_CONFIG, []).
	
start_link(Config) ->
	gen_server:start_link(?MODULE, Config, []).

get_key(Pid, Key) ->
	Config = get_config(Pid),
	get(Key, Config).
	
parse(Key, Config) ->
	get(Key, Config).
	
get_config(Pid) ->
  gen_server:call(Pid, {get_config}).
  
set_config(Pid, Config) ->
  gen_server:call(Pid, {set_config, Config}).

stop(Pid) ->
    gen_server:call(Pid, {stop}).


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
init(Config) ->
    {ok, #config_state{config=Config}}.

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

handle_call(get_config, _From, State) ->
	{reply, State#config_state.config, State};
	
handle_call({set_config, Config}, _From, State) ->
  {reply, ok, State#config_state{config=Config}};

handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State};

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
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end 
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end 
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

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

get(Key, Arr) ->
	Out = [ T || T <- Arr, element(1, T) =:= Key],
	case Out of
		[] ->
			{error, no_key};
		[Val] ->
			element(2, Val);
		[_Val|Vals] ->
			get(Key, Vals)
	end.
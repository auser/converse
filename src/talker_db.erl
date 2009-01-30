-module (talker_db).
-compile(export_all).

-include("talker.hrl").

-include_lib("stdlib/include/qlc.hrl").

start() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	try
		mnesia:table_info(erlang:hd(?DATABASES), type)
	catch
		exit: _ ->
			[io:format("Creating table ~p~n", [D]) || D <- ?DATABASES],
			[mnesia:create_table(D, [{attributes, 
				record_info(fields, node)},{type, set}, {disc_copies, [node()]}]) || D <- ?DATABASES]
	end,
	mnesia:wait_for_tables(?DATABASES, 20000).

lookup(Key) ->
	mnesia:transaction(fun() -> mnesia:read(node, {Key}) end).

find_node(Address, Port) ->
	do(qlc:q([X || X <-mnesia:table(node), X#node.key =:= {Address, Port} ])).
	
insert_node(Address, Port, Pid, Socket, Tuple) ->
	Row = #node{key={Address, Port}, address=Address, port=Port, pid=Pid, socket=Socket, tuple=Tuple},
	io:format("Node row: ~p~n", [Row]),
	insert(Row).

insert(Value) ->
	mnesia:transaction(fun() -> mnesia:write(Value) end).

delete(Key) ->
	mnesia:transaction(fun() -> mnesia:delete(Key) end).

delete_node(Address, Port) ->
	Old = {node, {Address, Port}},
	delete(Old).

select_all(Tab) ->
	do(qlc:q([X || X <- mnesia:table(Tab)])).

delete_all(Tab) ->
	AllKeys = do(qlc:q([X#node.key || X <- mnesia:table(Tab)])),
	[delete(K) || K <- AllKeys].

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

% construct_node(Key, Obj) ->
% 	#node{ key=Key, Obj }.

build_struct(Record) ->
    {struct, [
        {<<"address">>, list_to_binary(Record#node.address)},
		{<<"port">>, list_to_binary(Record#node.port)},
		{<<"pid">>, list_to_binary(Record#node.pid)},
		{<<"socket">>, list_to_binary(Record#node.socket)},
		{<<"tuple">>, list_to_binary(Record#node.tuple)}
    ]}.
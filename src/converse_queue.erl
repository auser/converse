-module (converse_queue).
-include ("converse.hrl").
-compile(export_all).

maybe_start_queue_timer(ok) -> ok;
maybe_start_queue_timer(queue) ->
	erlang:start_timer(?SLOWDOWN_INTERVAL, self(), catchup_timeout).

message(Name, Message) ->
	mnesia:dirty_write(Name, {Name, now(), Message}).
	
maybe_wait_for_queue_table(false, _) -> ok;
maybe_wait_for_queue_table(true, Reg_name) ->
	mnesia:wait_for_tables([Reg_name], infinity).

anything_in_queue(false, Reg_name) -> ok;
anything_in_queue(true, Reg_name) ->
	case mnesia:dirty_first(Reg_name) of
		'$end_of_table' -> ok;
		Else -> queue
	end.

maybe_create_queue_table(false, _) -> false;
maybe_create_queue_table(true, Name) ->
	case catch mnesia:table_info(Name, storage_type) of
	{'EXIT', {aborted, {no_exists, Name, storage_type}}} ->
		case mnesia:create_table(Name, [{disc_copies, [node()]}, 
																		{attributes, record_info(fields, converse_message_queue)}, 
																		{type, ordered_set}]) of
			{atomic, ok} -> true;
		Else ->
			error_logger:format("converse app cannot create queue table", []),
			false
		end;
	Ok ->
		true
	end.
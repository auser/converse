-module (utils).
-compile (export_all).

safe_integer(Int) ->
	case erlang:is_atom(Int) of
		true -> 
			erlang:list_to_integer(erlang:atom_to_list(Int));
		false -> 
			Int
	end.

live_process_named(Name) ->
	erlang:whereis(Name).
-module (converse_utils).
-compile (export_all).

get_registered_name_for_address(Proto, Type, Addr) ->	registered_name(Proto,Type,Addr).
	
registered_name(Type, From, Name) when is_atom(Type), is_atom(From), is_list(Name) -> 
	list_to_atom("converse."++atom_to_list(Type)++"."++atom_to_list(From)++"."++Name).
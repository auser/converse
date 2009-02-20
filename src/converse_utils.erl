-module (converse_utils).
-compile (export_all).

get_registered_name_for_address(Proto, Type, Addr) ->	
	IpName = inet_parse:ntoa(Addr),
	registered_name(Proto,Type,IpName).
	
registered_name(Type, From, Name) -> list_to_atom("converse."++atom_to_list(Type)++"."++atom_to_list(From)++"."++Name).
-module (converse_utils).
-compile (export_all).
-include_lib("kernel/include/inet.hrl").

get_registered_name_for_address(Proto, Type, Addr) ->	registered_name(Proto,Type,Addr).
	
registered_name(Type, From, Name) when is_atom(Type), is_atom(From), is_list(Name) -> 
	list_to_atom("converse."++atom_to_list(Type)++"."++atom_to_list(From)++"."++Name).
	
my_ip() ->
	{ok, Hostname} = inet:gethostname(),
  case inet:gethostbyname(Hostname) of
      {ok, #hostent{h_addr_list = Addrs}} -> erlang:hd(Addrs);
      {error, _Reason} -> Hostname
  end.
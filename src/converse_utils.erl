-module (converse_utils).
-compile (export_all).

registered_name(Name, Type) ->
  list_to_atom(atom_to_list(Name) ++ "." ++ Type).
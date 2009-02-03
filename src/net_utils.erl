-module (net_utils).
-compile (export_all).

ensure_server_running(Module, Fun) ->
	case whereis(Module) of
		{error} -> spawn(Fun);
		_ -> ok
	end,
	ok.
-module (converse_utils).
-compile (export_all).
	
get_child_pid(Name, Key) ->
	L = supervisor:which_children(Name),
	{value, {_, Pid, _, _}} = lists:keysearch(Key, 1, L),
	Pid.
	
safe_integer(Arg) ->
	Int = case erlang:is_list(Arg) of
		true -> erlang:hd(Arg);
		false -> Arg
	end,
	case erlang:is_atom(Int) of
		true -> 
			erlang:list_to_integer(erlang:atom_to_list(Int));
		false -> 
			case erlang:is_list(Int) of
				true ->
					erlang:list_to_integer(Int);
				false ->
					Int
			end
	end.

% Take an array of the form
% [ben awesome frank almost_awesome bob boring]
% and turn it into an array of the form
% [{ben, awesome}, {frank, almost_awesome}, {bob, boring}]
parse_args(Array) -> parse_args0(Array, []).	

parse_args0([], Acc) -> Acc;
parse_args0(Array, Acc) ->
	case length(Array) rem 2 of
		0 ->
			[Key|Rest] = Array, [Value|More] = Rest,
			NewAcc = lists:append(Acc, [{Key, Value}]),
			parse_args0(More, NewAcc);
		_ ->
			Acc
	end.

% Get the environment
get_app_env(Opt, Config, Default) ->
    case application:get_env(application:get_application(), Opt) of
    {ok, Val} -> Val;
    _ ->
			case init:get_argument(Opt) of
				[[Val|_]] ->
					Val;
				_Anything ->
					case config:parse(Opt, Config) of
						nil -> Default;
						Retriveved -> Retriveved
					end
			end
    end.
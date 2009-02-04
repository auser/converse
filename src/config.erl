-module (config).

%% API
-export([parse/2, update/3]).

parse(Key, Config) ->
	get(Key, Config).

update(Key, Value, Config) ->
	NewConfig = [ T || T <- Config, element(1, T) =/= Key],
	lists:append(NewConfig, [{Key, Value}]).

get(Key, Arr) ->
	Out = [ T || T <- Arr, element(1, T) =:= Key],
	case Out of
		[] ->
			{};
		[Val] ->
			element(2, Val);
		[_Val|Vals] ->
			get(Key, Vals)
	end.
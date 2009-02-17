-module (config).

%% API
-export([parse/2, update/3, delete/2, append/2]).

parse(Key, Config) ->
	get(Key, Config).

update(Key, Value, Config) ->
	NewConfig = [ T || T <- Config, element(1, T) =/= Key],
	lists:append(NewConfig, [{Key, Value}]).	
	
delete(Key, Config) ->
	[ T || T <- Config, element(1, T) =/= Key].

append(Config, Other) ->
	DFun = fun(Key, Value, Array) -> lists:append(delete(Key, Array), [{Key, Value}]) end,
	[NewConfig] = [ DFun(Key, Value, Config) || {Key, Value} <- Other ],
	NewConfig.

get(Key, Arr) ->
	Out = [ T || T <- Arr, element(1, T) =:= Key],
	case Out of
		[] ->
			nil;
		[Val] ->
			element(2, Val);
		[_Val|Vals] ->
			get(Key, Vals);
		Else ->
			Else
	end.
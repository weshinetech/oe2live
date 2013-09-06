-module(configs).
-compile(export_all).
-include_lib("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

get(K) ->
	from_cache(K, cache:get(K)).

from_cache(K, undefined) ->
	Value = default(K),
	cache:store(K, Value),
	Value;
from_cache(_, V) ->
	V.

default(Key) ->
	case file:consult("etc/oe2agentconfig") of
		{ok, List} ->
			cache_config(List),
			cache:get(Key);
		_ -> []
	end.

cache_config(List) ->
	lists:foreach(fun({K, V}) -> cache:store(K, V) end, List).
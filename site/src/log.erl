-module(log).
-compile(export_all).
-include_lib("records.hrl").

log(Log) ->
	log(cache:get(enable_logs), Log).

log(true, Log) ->
	Log1 = lists:flatten(io_lib:format("~s: ~s", [helper:epochtimetostring(helper:epochtime()), Log])),
	helper:print(Log1);
log(_, _) -> ok.

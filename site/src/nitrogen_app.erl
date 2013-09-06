-module(nitrogen_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    nitrogen_sup:start_link(),
		cache:start().

stop(_State) ->
    ok.

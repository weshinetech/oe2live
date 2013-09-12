-module(cache).
-behavior(gen_server).
-define(SERVER, cache_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/0, store/2, erase/1, get/1, stop/0, session_id/2, session_id/1]).

start() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:call(?SERVER, terminate).

store(Key, Value) ->
	gen_server:call(?SERVER, {store, Key, Value}).

erase(Key) ->
	gen_server:call(?SERVER, {erase, Key}).

get(Key) ->
	gen_server:call(?SERVER, {get, Key}).

init([]) ->
	Cache = dict:new(), {ok, Cache}.

handle_call({store, Key, Value}, _From, Cache) ->
	{reply, ok, dict:store(Key, Value, Cache)};

handle_call({erase, Key}, _From, Cache) ->
	{reply, ok, dict:erase(Key, Cache)};

handle_call({get, Key}, _From, Cache) ->
	try
		{reply, dict:fetch(Key, Cache), Cache}
	catch
		_:_ -> 
		{reply, undefined, Cache}
	end;

handle_call(terminate, _From, Cache) ->
	{stop, normal, ok, Cache}.

handle_cast(_Message, Cache) ->
	{noreply, Cache}.

handle_info(_Message, Cache) ->
	{noreply, Cache}.

terminate(_Reason, _Cache) ->
	ok.

code_change(_OldVersion, Cache, _Extra) ->
	{ok, Cache}.

session_id(Key, SessionId) ->
	cache:store(Key, SessionId).

session_id(Key) ->
	cache:get(Key).

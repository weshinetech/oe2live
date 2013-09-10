-module(helper).
-compile(export_all).
-include_lib("records.hrl").

a2l(X) ->
	atom_to_list(X).

l2a(undefined) ->
	undefined;
l2a(X) ->
	list_to_atom(X).

b2l(undefined) ->
	undefined;
b2l(X) ->
	binary_to_list(X).

l2b(undefined) ->
	undefined;
l2b(X) ->
	list_to_binary(X).

s2i(X) ->
	{I, _} = string:to_integer(X), I.

i2s(X) ->
	integer_to_list(X).

s2n(L) ->
	try list_to_integer(L)
	catch
		error:badarg ->	list_to_float(L)
	end.

n2s(N) ->
	try io_lib:format("~.2f",[N])
	catch
		error:badarg ->	integer_to_list(N)
	end.

time_s2t(undefined) ->
	undefined;
time_s2t(S) ->
	HMS = lists:foldl(fun(T, Acc) ->
		Acc ++ [s2i(T)]
	end, [], string:tokens(S, ":")),
	HMS.
time_t2s(undefined) ->
	undefined;
time_t2s([H, M, S]) ->
	lists:flatten(io_lib:format("~2..0B:~2..0B:~2..0B", [H, M, S])).

date_s2d(undefined) ->
	undefined;
date_s2d(D) ->
	Ymd = lists:foldl(fun(T, Acc) ->
		Acc ++ [s2i(T)]
	end, [], string:tokens(D, "-")),
	Ymd ++ [0, 0, 0].

date_d2s(undefined) ->
	undefined;
date_d2s([Y, M, D, _, _, _]) ->
	lists:flatten(io_lib:format("~p-~2..0B-~2..0B", [Y, M, D])).

date_toseconds([Y, M, D, H, Mi, S]) ->
	calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, Mi, S}}).

date_today() ->
	{Y, M, D} = date(),
	{H, Mi, S} = time(),
	[Y, M, D, H, Mi, S].

time_now() ->
	{H, M, S} = time(),
	[H, M, S].

a2dbid(X) ->
	l2b(a2l(X)).

dbid2a(X) ->
	l2a(b2l(X)).

a2b(A) ->
	l2b(a2l(A)).

b2a(A) ->
	l2a(b2l(A)).

print(X) ->
	io:format("-> ~p~n", [X]).

id(undefined, BaseId) -> BaseId;
id(ParentId, BaseId) -> helper:l2a(helper:a2l(ParentId) ++ helper:a2l(BaseId)).

id(#field {id=oeuserid}) -> '_id';
id(#field {type=primarykey}) -> '_id';
id(#field {baseid=BId}) when BId /= undefined -> BId;
id(#field {id=I}) -> I.

redirect_from_login() ->
	wf:redirect_from_login("/").

redirect("/login") ->
	wf:clear_session(),
	myauth:clear_session(),
	wf:redirect("/login");
redirect(Other) ->
	wf:redirect(Other).

session(K, V) -> wf:session(K, V).
session(K) -> wf:session(K).

state(K, V) -> wf:state(K, V).
state(K) -> wf:state(K).

httpget(Url) ->
	case httpc:request(Url) of
		{ok, {{_, 200, _}, _, Body}} -> Body;
		_ -> {error, request_failed}
	end.

sort(L) -> 
	lists:sort(fun(A, B) -> A < B end, L).

listupdate(K, V, L) ->
	case proplists:get_value(K, L) of
		undefined -> L ++ [{K, [V]}];
		Val -> lists:keyreplace(K, 1, L, {K, Val ++ [V]})
	end.

random_string(Len) ->
	Chrs = list_to_tuple("ABCDEFGHJKLMNPQRSTUVWXYZabcdefghjkmnopqrstuvwxyz23456789"),
	ChrsSize = size(Chrs),
	random:seed(erlang:now()),
	F = fun(_, R) -> [element(random:uniform(ChrsSize), Chrs) | R] end,
	lists:foldl(F, "", lists:seq(1, Len)).

ifundefempty(undefined) -> [];
ifundefempty(Other) -> Other.

uid() ->
	{A, B, C} = erlang:now(),
	l2a(lists:flatten(io_lib:format("~p-~p-~p", [A, B, C]))).

epochtime() ->
	calendar:datetime_to_gregorian_seconds(calendar:now_to_universal_time(now()))-719528*24*3600.

epochtimetostring(error) -> "-";
epochtimetostring([]) -> "-";
epochtimetostring(Seconds) -> 
	{{Year, Month, Day}, {Hour, Min, Sec}} = calendar:gregorian_seconds_to_datetime(Seconds + 719528*24*3600 + 5*3600 + 1800),
	lists:flatten(io_lib:fwrite("~4B/~B/~B ~2B:~2.10.0B:~2.10.0B",	[Year, Month, Day, Hour, Min, Sec])).

abra(S) ->
	helper:b2l(base64:encode(crypto:block_encrypt(aes_cfb128, <<"abcdefghabcdefgh">>, <<"12345678abcdefgh">>, helper:l2b(S)))).

dbra(S) ->
	helper:b2l(crypto:block_decrypt(aes_cfb128, <<"abcdefghabcdefgh">>, <<"12345678abcdefgh">>, base64:decode(S))).
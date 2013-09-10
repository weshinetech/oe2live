-module(validators).
-compile(export_all).
-include_lib("records.hrl").

get(username) ->
	fun(_, V) ->
		profiles:exists(V) == false
	end;

get(required) ->
	fun(_, V) ->
		case V of
			[] -> false;
			_ -> true
		end
	end;

get(integer) ->
	fun(_, V) ->
		case string:to_integer(V) of
			{_, []} -> true;
			_ -> false
		end
	end;

get(length4) ->
	fun(_, V) ->
		case V of
			undefined -> false;
			_ -> length(V) > 3
		end
	end;

get(length5) ->
	fun(_, V) ->
		case V of
			undefined -> false;
			_ -> length(V) > 4
		end
	end;

get(length8) ->
	fun(_, V) ->
		case V of
			undefined -> false;
			_ -> length(V) > 7
		end
	end;

get(noallspaces) ->
	fun(_, V) ->
		case re:replace(V, "(^\\s+)|(\\s+$)", "", [global,{return,list}]) of
			[] -> false;
			_ -> true
		end
	end;

get(ymd_months) -> get(range_integer, -1, 13);
get(ymd_days) -> get(range_integer, -1, 32);

get(mobile) ->
	fun(_, V) ->
		case string:to_integer(V) of
			{error, _} -> false;
			{N, _} when N > 1000000000, N < 9999999999 -> true;
			_ -> false
		end
	end;

get(email) ->
	fun(_, V) ->
		case re:run(V, "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]+") of
			{match, _} -> true;
			_ -> false
		end
	end;

get(date) ->
	fun(_, V) ->
		try
			Seconds = helper:date_toseconds(helper:date_s2d(V)),
			is_integer(Seconds)
		catch
			_:_ -> false
		end
	end;	

get(time) ->
	fun(_, V) ->
		try
			[H, M, S] = helper:time_s2t(V),
			H*60*60 + M*60 + S < (24*60*60 + 1)
		catch
			_:_ -> false
		end
	end;	

get(dob) ->
	fun(_, V) ->
		TodaySecs = helper:date_toseconds(helper:date_today()),
		Offset = 16*365*24*60*60,
		DateSecs = helper:date_toseconds(helper:date_s2d(V)),
		(TodaySecs - Offset) > DateSecs
	end;

get(pincode) ->
	?MODULE:get(integer);

get(ip_address) ->
	fun(_, V) ->
		try
			[A, B, C, D] = string:tokens(V, "."),
			Fn = fun(X) ->
				(helper:s2i(X) > -1) and (helper:s2i(X) < 256)
			end,
			Fn(A) and Fn(B) and Fn(C) and Fn(D)
		catch
			_:_ -> false
		end
	end;	
	
get(Validator) ->
	helper:print(Validator),
	throw (validator_does_not_exist).

get(integer_or, Allowed) ->
	fun(Tag, V) ->
		case V of
			Allowed -> true;
			_ -> FInteger = ?MODULE:get(integer), FInteger(Tag, V)
		end
	end.

get(range_integer, Mi, Mx) ->
	fun(_, V) ->
		case string:to_integer(V) of
			{error, _} -> false;
			{N, _} when N > Mi, N < Mx -> true;
			_ -> false
		end
	end.

tip(ip_address) -> locale(ip_address);
tip(coursedisplayid) -> locale(coursedisplayid);
tip(username) -> locale(username);
tip(mobile) -> locale(mobile);
tip(email) -> locale(email);
tip(pincode) -> locale(pincode);
tip(score_scored) -> locale(score_scored);
tip(score_outof) -> locale(score_outof);
tip(date) -> locale(date);
tip(dob) -> locale(dob);
tip(length8) -> locale(length8);
tip(length5) -> locale(length5);
tip(noallspaces) -> locale(noallspaces);
tip(_) -> locale(required).

locale(Id) -> locale:get(helper:l2a("validator_" ++ helper:a2l(Id))).

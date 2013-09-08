-module (download).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	case myauth:is_valid_session() and myauth:is_valid_access(?MODULE) of
		false -> 
			helper:redirect("/unauthorised");
		_ -> 
			download()
	end.

download() ->
	try
		download(wf:q(type), wf:q(testid))
	catch
		_:_ ->
			helper:redirect("/index")
	end.

download("results", TestId) ->
	FileName = getfilename(wf:q(type), TestId),
	{Y, _, C} = oeusers:getusers_by_state(TestId),
	Res = lists:map(fun(U) ->
		getlines(U, [oeusercentercode, oeuserseatnumber, oeuserfullname, oeuserscore])
	end, [C, Y]),
	Data = string:join(Res, "\n"),
	wf:content_type("multipart/form-data"),
	wf:header("Content-Disposition", "filename=" ++ FileName),
	Data;

download("tokens", TestId) ->
	FileName = getfilename(wf:q(type), TestId),
	Users = oeusers:getusers(TestId),
	Data = getlines(Users, [oeusercentercode, oeuserseatnumber, oeuserfullname, oeusertoken]),
	wf:content_type("multipart/form-data"),
	wf:header("Content-Disposition", "filename=" ++ FileName),
	Data;

download(_, _) ->
	[].
	
getfilename(Type, TestId) ->
	Fs = oe2tests:get(TestId),
	Name = fields:finduival(Fs, testname),
	Date = fields:finduival(Fs, testdate),
	string:to_upper(string:join([Type, Date, Name], "_") ++ ".csv").

getlines(List, Ids) ->
	Res = lists:map(fun(L) -> getline(L, Ids)	end, List),
	string:join(Res, "\n").

getline(L, Ids) ->
	Res = lists:map(fun(I) ->
		fields:finduival(L, I)
	end, Ids),
	string:join(Res, ",").
-module (print).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	case myauth:is_valid_session() and myauth:is_valid_access(?MODULE) of
		false -> 
			helper:redirect("/unauthorised");
		_ -> 
			print()
	end.

print() ->
	try
		print(wf:q(type), wf:q(testid))
	catch
		_:_ ->
			helper:redirect("/index")
	end.

print("tokens", TestId) ->
	HtmlFile = getHtml("tokens"),
	Users = oeusers:getusers(TestId),
	TestDetails = getTestDetails(TestId),
	Data = layout:table_s(Users, [oeuserseatnumber, oeuserfullname, oeusertoken], 2, []),
	io_lib:format(HtmlFile, [locale:get(tokens), configs:get(customer_image), configs:get(customer_text), TestDetails, Data, locale:get(footer_copyright)]);

print("results", TestId) ->
	HtmlFile = getHtml("results"),
	Users = oeusers:getusers(TestId),
	TestDetails = getTestDetails(TestId),
	Data = layout:table_s(Users, [oeuserseatnumber, oeuserfullname, oeuserscore], 2, locale:get(msg_absent)),
	io_lib:format(HtmlFile, [locale:get(tokens), configs:get(customer_image), configs:get(customer_text), TestDetails, Data, locale:get(footer_copyright)]);

print(_, _) ->
	[].
	
getfilename(Type, TestId) ->
	Fs = oe2tests:get(TestId),
	Name = fields:finduival(Fs, testname),
	Date = fields:finduival(Fs, testdate),
	string:to_upper(string:join([Type, Date, Name], "_") ++ ".html").

getlines(List, Ids) ->
	Res = lists:map(fun(L) -> getline(L, Ids)	end, List),
	string:join(Res, "\n").

getline(L, Ids) ->
	Res = lists:map(fun(I) ->
		fields:finduival(L, I)
	end, Ids),
	string:join(Res, ",").

getHtml(Type) ->
	case file:read_file("site/templates/" ++ Type ++ ".html") of
		{ok, Contents} -> helper:b2l(Contents);
		_ -> []
	end.

getTestDetails(TestId) ->
	Fs = oe2tests:get(TestId),
	string:join([fields:finduival(Fs, testname), fields:finduival(Fs, testdate)] , ",").
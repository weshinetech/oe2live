-module (tokens).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(tokens_title).

heading() ->
	locale:get(tokens_heading).

layout() ->
	helper_ui:fullpage(layout(wf:q(oetestid))).

layout(undefined) ->
	helper:redirect("/index");
layout(TestId) ->
	T = oe2tests:get(TestId),
	Users = oeusers:getusers(TestId),
	Actions = [
		helper_ui:print_button(),
		helper_ui:download_button()
	],
	TestTable = layout:table_fields(T, [oetestcourseid, testname, testdate]),
	TokensTable = layout:table(Users, [oeuserseatnumber, oeuserfullname, oeusertoken], 2),
	[
		Actions, TestTable, TokensTable
	].

event(download) ->
	Url = "/download?type=tokens&testid=" ++ wf:q(oetestid),
	helper:redirect(Url);

event(Event) ->
	helper:print(Event).
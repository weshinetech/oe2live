-module (report_generate).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(report_generate_title).

heading() ->
	locale:get(report_generate_heading).

%-----------------------------------------------------------------------------------------------
% LAYOUTS
%-----------------------------------------------------------------------------------------------
layout() ->
	T = oe2tests:get(wf:q(oetestid)),
	helper_ui:fullpage(check_result_generation(T, fields:getuivalue(T, test_agent_result_generate))).

%-----------------------------------------------------------------------------------------------
% CHECK RESULT GENERATION
%-----------------------------------------------------------------------------------------------
check_result_generation(TFs, "yes") ->
	Res = oeusers:getusers_by_state(fields:getuivalue(TFs, '_id')),
	checkactive(TFs, Res);
check_result_generation(TFs, _) ->
	[
		layout:table_fields(TFs, [oetestcourseid, testname, testdate]),
		locale:get(msg_generate_report_disabled)
	].

%-----------------------------------------------------------------------------------------------
% CHECK ACTIVE
%-----------------------------------------------------------------------------------------------
checkactive(_, {_, A, _}) when length(A) > 0 ->
	locale:get(msg_generate_report_error_active);
checkactive(FTest, Users) ->
	checksync(FTest, Users, helper:httpget(url_state(FTest))).

%-----------------------------------------------------------------------------------------------
% CHECK SYNC
%-----------------------------------------------------------------------------------------------
checksync(FTest, Users, {error, _}) ->
	results(FTest, Users, locale:get(msg_generate_report_error_request_failed));

checksync(FTest, {Y, A, C} = Users, Res) ->
	SyncE = try
		[Ys, As, Cs] = string:tokens(Res, ","),
		case (helper:s2i(Ys) == length(Y)) and (helper:s2i(As) == length(A)) and (helper:s2i(Cs) == length(C)) of
			true -> [];
			false -> locale:get(msg_generate_report_error_data_sync)
		end
	catch
		_:_ -> locale:get(msg_generate_report_error_data_sync_ex)
	end,
	results(FTest, Users, SyncE).

%-----------------------------------------------------------------------------------------------
% RESULTS
%-----------------------------------------------------------------------------------------------
results(FTest, {_, _, C}, SyncE) ->
	TId = fields:getuivalue(FTest, '_id'),
	oeusers:compute_scores(TId, C),
	SyncENew = case SyncE of
		[] -> [];
		_ -> #panel {class="well hidden-print", body=SyncE}
	end,
	Actions = [
		helper_ui:print_button(),
		helper_ui:download_button()
	],
	Users = oeusers:getusers(TId),
	[
		SyncENew,
		Actions,
		layout:table_fields(FTest, [oetestcourseid, testname, testdate]),
		layout:table(Users, [oeuserseatnumber, oeuserfullname, oeuserscore], 2)
	].

%-----------------------------------------------------------------------------------------------
% EVENTS
%-----------------------------------------------------------------------------------------------
event(download) ->
	Url = "/download?type=results&testid=" ++ wf:q(oetestid),
	helper:redirect(Url);

event(Event) ->
	helper:print(Event).

%-----------------------------------------------------------------------------------------------
% HELPERS
%-----------------------------------------------------------------------------------------------
url_state(FTest) ->
	configs:get(mainserver) ++ "/oetest_state?oetestid=" ++ fields:getuivalue(FTest, '_id') ++ "&oecentercode=" ++ myauth:oecentercode().

%-----------------------------------------------------------------------------------------------
% END
%-----------------------------------------------------------------------------------------------

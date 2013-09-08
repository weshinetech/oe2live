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

layout() ->
	helper_admin:layout(?MODULE).

layout(report_generate) ->
	{Fs, Es} = layout:get(?CREATE, helper_ui:fields(?MODULE), helper_ui:events(eids())),
	[
		layout:g(10, layout:form(oe2form_simple, ?MODULE, {Fs, Es})),
		layout:g(10, #panel {class="well", body=[
			#panel {body=locale:get(msg_generate_report_declare)},
			#br{},
			#panel {body=locale:get(msg_generate_report_declare_over)},
			#panel {body=locale:get(msg_generate_report_declare_sync)},
			#br{},
			#panel {body=fullname()}
		]}),
		layout:g(10, #panel {id=result, body=[]})
	].

fids() -> [
	testsactive
].

eids() -> [
	show_create
].

event({timer_flash, _} = E) ->
	helper_ui:event(E);

event(show_create) ->
	[FTest] = fields:uivalue(helper_ui:fields(?MODULE)),
	Res = oeusers:getusers_by_state(FTest#field.uivalue),
	checkactive(FTest, Res);

event(Event) ->
	helper:print(Event).

status(Y, A, C) ->
	Ids = [oeusercentercode, oeuserseatnumber, oeuserfullname, oeuserscore],
	#table {
		class="table table-bordered table-hover",
		rows=helper_admin:layout_oeuser_header(Ids) ++
			helper_admin:layout_oeuser_rows(A, Ids, "") ++ 
			helper_admin:layout_oeuser_rows(C, Ids, "") ++ 
			helper_admin:layout_oeuser_rows(Y, Ids, "")
	}.

fullname() ->
	Fs = myauth:userfields(),
	io_lib:format("~s ~s ~s", [fields:finduival(Fs, firstname), fields:finduival(Fs, middlename), fields:finduival(Fs, lastname)]).

checkactive(_, {_, A, _}) when length(A) > 0 ->
	helper_ui:flash(error, locale:get(msg_generate_report_error_active));
checkactive(FTest, Users) ->
	checksync(FTest, Users).

checksync(FTest, {Y, A, C}) ->
	Url = url_state(FTest),
	Res = helper:httpget(Url),
	checksync_1(FTest, {Y, A, C}, Res).

checksync_1(_, {_, _, _}, {error, _}) ->
	helper_ui:flash(error, locale:get(msg_generate_report_error_request_failed));
checksync_1(FTest, {Y, A, C}, Res) ->
	try
		[Ys, As, Cs] = string:tokens(Res, ","),
		case (helper:s2i(Ys) == length(Y)) and (helper:s2i(As) == length(A)) and (helper:s2i(Cs) == length(C)) of
			true ->
				helper:httpget(url_completed(FTest)),
				compute_scores(FTest, C);
			false ->
				helper_ui:flash(error, locale:get(msg_generate_report_error_data_sync))
		end
	catch
		_:_ -> helper_ui:flash(error, locale:get(msg_generate_report_error_data_sync_ex))
	end.

compute_scores(FTest, C) ->
	oeusers:compute_scores(FTest#field.uivalue, C),
	showresults(FTest).

showresults(FTest) ->
	{Y, A, C} = oeusers:getusers_by_state(FTest#field.uivalue),
	wf:update(result, #panel {body=status(Y, A, C)}).

url_state(FTest) ->
	configs:get(mainserver) ++ "/oetest_state?oetestid=" ++ FTest#field.uivalue ++ "&oecentercode=" ++ myauth:oecentercode().

url_completed(FTest) ->
	configs:get(mainserver) ++ "/oetest_completed?oetestid=" ++ FTest#field.uivalue ++ "&oecentercode=" ++ myauth:oecentercode().
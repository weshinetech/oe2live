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
		layout:g(10, #panel {id=sync_status, body=[]}),
		layout:g(10, #panel {id=download, body=[]}),
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

event({download, FTest}) ->
	Url = "/download?type=results&testid=" ++ FTest#field.uivalue,
	helper:redirect(Url);

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
	case helper:httpget(Url) of
		{error, _} ->
			updatesyncstatus(locale:get(msg_generate_report_error_request_failed));
		Res ->
			showsyncstatus(FTest, {Y, A, C}, Res)
	end,
	oeusers:compute_scores(FTest#field.uivalue, C),
	show_download(FTest),
	showresults(FTest).

showsyncstatus(FTest, {Y, A, C}, Res) ->
	try
		[Ys, As, Cs] = string:tokens(Res, ","),
		case (helper:s2i(Ys) == length(Y)) and (helper:s2i(As) == length(A)) and (helper:s2i(Cs) == length(C)) of
			true ->
				helper:httpget(url_completed(FTest)),
				wf:update(sync_status, []);
			false ->
				updatesyncstatus(locale:get(msg_generate_report_error_data_sync))
		end
	catch
		_:_ -> updatesyncstatus(locale:get(msg_generate_report_error_data_sync_ex))
	end.

updatesyncstatus(Message) ->
	wf:update(sync_status, #panel {class="well label-important", body=Message}).

showresults(FTest) ->
	{Y, A, C} = oeusers:getusers_by_state(FTest#field.uivalue),
	wf:update(result, #panel {body=status(Y, A, C)}).

url_state(FTest) ->
	configs:get(mainserver) ++ "/oetest_state?oetestid=" ++ FTest#field.uivalue ++ "&oecentercode=" ++ myauth:oecentercode().

url_completed(FTest) ->
	configs:get(mainserver) ++ "/oetest_completed?oetestid=" ++ FTest#field.uivalue ++ "&oecentercode=" ++ myauth:oecentercode().

show_download(FTest) ->
	wf:update(download, [
		#link {postback={download, FTest}, text=locale:get(msg_download)},
		#span {text=" - "},
		#link {url="/print?type=results&testid=" ++ FTest#field.uivalue, text=locale:get(msg_print)}
	]).
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
		layout:g(10, #panel {id=result, body=[]})
	].

fids() -> [
	testsactive
].

eids() -> [
	show_create
].

event(show_create) ->
	[FTest] = fields:uivalue(helper_ui:fields(?MODULE)),
	oeusers:compute_scores(FTest#field.uivalue),
	{Y, A, C} = oeusers:getusers_by_state(FTest#field.uivalue),
	wf:update(result, #panel {body=status(Y, A, C)});

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
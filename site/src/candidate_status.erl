-module (candidate_status).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(candidate_status_title).

heading() ->
	locale:get(candidate_status_heading).

layout() ->
	helper_admin:layout(?MODULE).

layout(candidate_status) ->
	{Fs, Es} = layout:get(?CREATE, helper_ui:fields(?MODULE), helper_ui:events(eids())),
	[
		layout:g(10, layout:form(oe2form_simple, ?MODULE, {Fs, Es})),
		layout:g(10, #panel {id=result_summary, body=[]}),
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
	{Y, A, C} = oeusers:getusers_by_state(FTest#field.uivalue),
	wf:update(result, #panel {body=[
		summary(Y, A, C),
		status(Y, A, C)
	]});

event(Event) ->
	helper:print(Event).

status(Y, A, C) ->
	Ids = [oeusercentercode, oeuserseatnumber, oeuserfullname, oeusertimeleftseconds, oeuserexamstate],
	#table {
		class="table table-bordered table-hover",
		rows=helper_admin:layout_oeuser_header(Ids) ++
			helper_admin:layout_oeuser_rows(A, Ids, "error") ++ 
			helper_admin:layout_oeuser_rows(C, Ids, "success") ++ 
			helper_admin:layout_oeuser_rows(Y, Ids, "")
	}.

summary(Y, A, C) ->
	#table {
		class="myfixedwidthtable table table-bordered table-hover",
		rows=[
			#tablerow {class="error", cells =[
				#tablecell {body=#span {text=locale:get(active)}},
				#tablecell {body=#span {text=length(A)}}
			]},
			#tablerow {class="success", cells=[
				#tablecell {body=#span {text=locale:get(completed)}},
				#tablecell {body=#span {text=length(C)}}
			]},
			#tablerow {class="", cells=[
				#tablecell {body=#span {text=locale:get(yettostart)}},
				#tablecell {body=#span {text=length(Y)}}
			]}
		]
	}.

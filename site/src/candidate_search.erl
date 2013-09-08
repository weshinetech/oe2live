-module (candidate_search).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(candidate_search_title).

heading() ->
	locale:get(candidate_search_heading).

layout() ->
	helper_admin:layout(?MODULE).

layout(candidate_search) ->
	{Fs, Es} = layout:get(?CREATE, helper_ui:fields(?MODULE), helper_ui:events(eids())),
	[
		layout:g(10, layout:form(oe2form_simple, ?MODULE, {Fs, Es})),
		layout:g(10, #panel {id=result, body=[]})
	].

fids() -> [
	testsactive,
	oeuserseatnumber
].

eids() -> [
	show_create
].

event(show_create) ->
	[FTest, FSNo] = fields:uivalue(helper_ui:fields(?MODULE)),
	onresult(oeusers:getuser(FTest#field.uivalue, FSNo#field.uivalue));

event(Event) ->
	helper:print(Event).

onresult({error, _}) ->
	wf:update(result, locale:get(msg_candidate_not_found));
onresult(Fields) ->
	Elements = [
		layout:table_fields(Fields, [
			oeusercentercode,
			oeuserseatnumber,
			oeuserfullname,
			oeuseraddname,
			oeuserlogintimes,
			oeusertoken,
			oeuserstarttime,
			oeuserendtime,
			oeusertimeleftseconds,
			oeuserscore, 
			oeuserexamstate
		], qnarows(fields:find(Fields, oeuserqna)))
	],
	wf:update(result, Elements).

qnarows(#field {uivalue=List}) ->
	T = length(List),
	U = length(lists:filter(fun({_, A}) -> A == "0" end, List)),
	A = T - U,
	[
		#tablerow {cells=[
			#tablecell {body=#span {text=locale:get(msg_total_questions)}},
			#tablecell {body=#span {text=T}}
		]},
		#tablerow {cells=[
			#tablecell {body=#span {text=locale:get(msg_attempted_questions)}},
			#tablecell {body=#span {text=A}}
		]},
		#tablerow {cells=[
			#tablecell {body=#span {text=locale:get(msg_unattempted_questions)}},
			#tablecell {body=#span {text=U}}
		]}
	].
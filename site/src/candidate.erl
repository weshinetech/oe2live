-module (candidate).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(candidate_title).

heading() ->
	locale:get(candidate_heading).

layout() ->
	Elements = onresult(oeusers:getuser(wf:q(oetestid), wf:q(oeuserseatnumber))),
	helper_ui:fullpage(Elements).

onresult({error, _}) ->
	wf:update(result, locale:get(msg_candidate_not_found));
onresult(Fields) ->
	T = oe2tests:get(wf:q(oetestid)),
	Action = case fields:getuivalue(Fields, oeuserexamstate) of
		"active" -> [
			#button {
				class="btn btn-primary hidden-print mylabel",
				postback=admin_exam_relogin_test_confirm,
				text=locale:get(admin_exam_relogin_test)
			},
			#button {
				class="btn btn-danger hidden-print mylabel",
				postback=admin_exam_submit_test_confirm,
				text=locale:get(exam_submit_test)
			}
		];
		_ ->
			[]
	end,
	TestT = layout:table_fields(T, [oetestcourseid, testname, testdate]),
	UserT = layout:table_fields(Fields, [
				oeuserseatnumber,
				oeusercentercode,
				oeuserfullname,
				oeuseraddname,
				oeuserlogintimes,
				oeusertoken,
				oeuserstarttime,
				oeuserendtime,
				oeusertimeleftseconds,
				oeuserscore, 
				oeuserexamstate,
				oeuserips
			], qnarows(fields:find(Fields, oeuserqna))),
	Rows = TestT#table.rows ++ UserT#table.rows,
	Elements = [
		#panel {body=Action},
		UserT#table {rows=Rows}
	],
	Elements.

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

event(admin_exam_submit_test_confirm) ->
	wf:wire(#confirm {text=locale:get(admin_exam_submit_test_confirm), postback=admin_exam_submit_test_confirm_ok});

event(admin_exam_relogin_test_confirm) ->
	wf:wire(#confirm {text=locale:get(admin_exam_relogin_test_confirm), postback=admin_exam_relogin_test_confirm_ok});

event(admin_exam_submit_test_confirm_ok) ->
	SN = wf:q(oeuserseatnumber),
	TI = wf:q(oetestid),
	Fs = oeusers:get(?DB_USERS ++ TI, SN),
	NewFs = lists:foldl(fun(F, Acc) ->
		NewF = case F#field.id of
			oeuserexamstate -> F#field {uivalue=?COMPLETED};
			oeuserendtime -> F#field {uivalue=helper:i2s(helper:epochtime())};
			_ -> F
		end,
		Acc ++ [NewF]
	end, [], Fs),
	save_user(TI, SN, NewFs);

event(admin_exam_relogin_test_confirm_ok) ->
	SN = wf:q(oeuserseatnumber),
	TI = wf:q(oetestid),
	Fs = oeusers:get(?DB_USERS ++ TI, SN),
	NewFs = lists:foldl(fun(F, Acc) ->
		NewF = case F#field.id of
			oeuserexamstate -> F#field {uivalue=?RELOGIN};
			_ -> F
		end,
		Acc ++ [NewF]
	end, [], Fs),
	save_user(TI, SN, NewFs);

event(_) ->
	ok.

save_user(TI, SN, NewFs) ->
	case oeusers:update(?DB_USERS ++ TI, NewFs) of
		{ok, _} ->
			cache:session_id(SN, undefined),
			helper:redirect("/candidate?oetestid=" ++ TI ++ "&oeuserseatnumber=" ++ SN);
		_ ->
			helper:flash({error, locale:get(msg_failed)})
	end.
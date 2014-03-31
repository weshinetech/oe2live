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
	helper_ui:fullpage(layout(wf:q(oetestid))).

layout(undefined) ->
	helper:redirect("/index");
layout(TestId) ->
	T = oe2tests:get(TestId),
	{Y, A, C} = oeusers:getusers_by_state(TestId),
	[
		summary(T, Y, A, C),
		status(Y, A, C)
	].

status(Y, A, C) ->
	Ids = [oeuserseatnumber, oeuserfullname, oeuserips, oeuserlogintimes, oeusertimeleftseconds, oeuserexamstate],
	#table {
		class="table table-bordered table-hover table-condensed",
		rows=helper_admin:layout_oeuser_header(Ids) ++
			helper_admin:layout_oeuser_rows(A, Ids, "error") ++ 
			helper_admin:layout_oeuser_rows(C, Ids, "success") ++ 
			helper_admin:layout_oeuser_rows(Y, Ids, "")
	}.

summary(T, Y, A, C) ->
	#table {
		class="myfixedwidthtable table table-bordered table-hover table-condensed",
		rows=[
			#tablerow {cells =[
				#tablecell {body=#span {text=locale:get(oetestcourseid)}},
				#tablecell {body=#span {text=fields:getuivalue(T, oetestcourseid)}}
			]},
			#tablerow {cells =[
				#tablecell {body=#span {text=locale:get(testname)}},
				#tablecell {body=#span {text=fields:getuivalue(T, testname)}}
			]},
			#tablerow {cells =[
				#tablecell {body=#span {text=locale:get(testdate)}},
				#tablecell {body=#span {text=fields:getuivalue(T, testdate)}}
			]},
			#tablerow {cells =[
				#tablecell {body=#span {text=locale:get(active)}},
				#tablecell {body=#span {text=length(A)}}
			]},
			#tablerow {cells=[
				#tablecell {body=#span {text=locale:get(completed)}},
				#tablecell {body=#span {text=length(C)}}
			]},
			#tablerow {cells=[
				#tablecell {body=#span {text=locale:get(yettostart)}},
				#tablecell {body=#span {text=length(Y)}}
			]}
		]
	}.

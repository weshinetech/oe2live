-module (index).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(index_welcome).

heading() ->
	locale:get(index_heading).

layout() ->
	Elements = #panel {id=result, body=locale:get(msg_please_wait)},
	wf:wire(#event{type=timer, delay=1000, postback=load_table}),
	helper_ui:fullpage(Elements).

layout_tests([]) ->
	locale:get(msg_index_no_active_tests);
layout_tests(Tests) ->
	#table {
		class="table table-bordered table-hover table-condensed",
		rows=table_header([
			testname,
			testdate,
			index_yettostart,
			index_active,
			index_completed,
			msg_tokens,
			msg_status,
			msg_results
		]) ++  table_rows(Tests)
	}.

table_header(Ids) -> [
	#tablerow {cells=lists:map(fun(I) -> #tableheader {body=locale:get(I)} end, Ids)}
].

table_rows(Tests) ->
	lists:map(fun(T) -> layout_row(T) end, Tests).

layout_row(T) ->
	{Y, A, C} = oeusers:getusers_by_state(fields:finduival(T, '_id')),
	#tablerow {cells=[
		#tablecell {body=[#span {text=fields:finduival(T, testname)}]},
		#tablecell {body=[#span {text=fields:finduival(T, testdate)}]},
		#tablecell {body=[#span {text=length(Y)}]},
		#tablecell {body=[#span {text=length(A)}]},
		#tablecell {body=[#span {text=length(C)}]},
		#tablecell {body=[
			#link {url="/tokens?oetestid=" ++ fields:finduival(T, '_id'), text=locale:get(view)}
		]},
		#tablecell {body=[
			#link {url="/candidate_status?oetestid=" ++ fields:finduival(T, '_id'), text=locale:get(view)}
		]},
		#tablecell {body=[
			#link {url="/report_generate?oetestid=" ++ fields:finduival(T, '_id'), text=locale:get(admin_link_report_generate)}
		]}
	]}.

event(load_table) ->
	wf:update(result, layout_tests(oe2tests:active()));

event(E) ->
	helper:print(E).

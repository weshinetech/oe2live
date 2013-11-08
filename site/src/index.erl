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
	helper_admin:layout(?MODULE).

layout(index) ->
	Elements = [
		#h4 {text=configs:get(customer_text)},
		#hr{},
		#panel {id=result, body=locale:get(msg_please_wait)}
	],
	wf:wire(#event{type=timer, delay=1000, postback=load_table}),
	Elements.

layout_tests(Tests) ->
	#table {
		class="table table-bordered table-hover",
		rows=table_header([testname, testdate, index_yettostart, index_active, index_completed, msg_tokens]) ++  table_rows(Tests)
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
			#link {postback={download, fields:finduival(T, '_id')}, text=locale:get(msg_download)},
			#span {text=" - "},
			#link {url="/print?type=tokens&testid=" ++ fields:finduival(T, '_id'), text=locale:get(msg_print)}
		]}
	]}.

event(load_table) ->
	wf:update(result, layout_tests(oe2tests:active()));

event({download, TestId}) ->
	Url = "/download?type=tokens&testid=" ++ TestId,
	helper:redirect(Url);

event(E) ->
	helper:print(E).
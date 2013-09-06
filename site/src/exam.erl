-module (exam).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

%---------------------------------------------------------------------------------------------------
% INIT MODULE
%---------------------------------------------------------------------------------------------------
main() ->
	case myauth:pageloaded(?MODULE) of
		true -> helper:redirect("/login");
		_ -> myauth:main(?MODULE)
	end.

title() ->
	locale:get(exam_title).

heading() ->
	locale:get(exam_heading).

layout() ->
	myauth:pageloaded(?MODULE, true),
	helper:state(questionindex, 1),
	Body = #panel {id=exam_page, body=layout_question()},
	loop_timer(helper:s2i(getuservalue(oeusertimeleftseconds))),
	Body.

%---------------------------------------------------------------------------------------------------
% TIMERS
%---------------------------------------------------------------------------------------------------
loop_timer(TimeLeft) when TimeLeft < 1 ->
	save_timer(0),
	helper:redirect("/login");
loop_timer(TimeLeft) ->
	Interval = 60,
	save_timer(TimeLeft),
	wf:update(exam_timer, layout_timer(TimeLeft)),
	wf:wire(#event{type=timer, delay=Interval*1000, postback={timer_exam, TimeLeft - Interval}}).

%---------------------------------------------------------------------------------------------------
% LAYOUTS
%---------------------------------------------------------------------------------------------------
layout_timer(TimeLeft) ->
	TimeFmt = case TimeLeft < 61 of
		true -> "< 1m";
		_ -> io_lib:format("~pm", [TimeLeft div 60])
	end,
	HighlightClass = if
		TimeLeft < 120 -> "label-important";
		TimeLeft < 660 -> "label-warning";
		true -> "label-info"
	end,
	#span {class="exam_timer_element label " ++ HighlightClass, text=TimeFmt, actions=#effect {effect=highlight}}.

%---------------------------------------------------------------------------------------------------
layout_question_list() ->
	QnA = getuservalue(oeuserqna), [
	#panel {body=[
		#span {class="mylabel label label-default", text=locale:get(exam_questions_list_table)}
	]},
	#br{},
	#table {class="table table-bordered", rows=layout_question_list_rows(QnA)}
].

layout_question_list_rows(List) ->
	{_, CellElements} = lists:foldl(fun({_, O}, {I, Cells}) ->
		{I+1, Cells ++ [layout_question_list_cells({I, O})]}
	end, {1, []}, List),
	COLUMNS = 6,
	ListOfCells = [lists:sublist(CellElements, X, COLUMNS) || X <- lists:seq(1, length(CellElements), COLUMNS)],
	lists:map(fun(Cs) ->
		Cs1 = case length(Cs) < COLUMNS of
			true -> Cs ++ filler_cells(COLUMNS - length(Cs));
			_ -> Cs
		end,
		#tablerow {cells=Cs1}
	end, ListOfCells).

layout_question_list_cells({Index, O}) ->
	BookMark = case is_question_bookmarked(Index) of
		true -> "<i class='icon-bookmark'></i>";
		false -> []
	end,
	Reported = case is_question_reported(Index) of
		true -> "<i class='icon-warning-sign'></i>";
		false -> []
	end,
	[
		#tablecell {body=[
			#link {class="exam_nav_link", text=io_lib:format("~2..0B", [Index]), postback={question_list, Index}},
			#span {class="mylabel label label-default", text=getoptiondisplay(O)},
			#span {class="mylabel label label-default", body=BookMark},
			#span {class="mylabel label label-important", body=Reported}
		]}
	].

filler_cells(N) ->
	lists:map(fun(_) -> #tablecell {body=""} end, lists:seq(1, N)).

%---------------------------------------------------------------------------------------------------
layout_question() ->
	{Id, _} = lists:nth(helper:state(questionindex), getuservalue(oeuserqna)),
	Fs = getquestionfields(Id),
	layout_question(Fs).

layout_question({error, _}) ->
	[];
layout_question(Fs) ->
	#panel {body=[
		#panel {body=layout_question_info(Fs)},
		#hr{},
		#panel {body=layout_question_desc(Fs)},
		#panel {body=layout_question_options(Fs)}
	]}.

%---------------------------------------------------------------------------------------------------
layout_question_info(Fs) ->
	Index = helper:state(questionindex),
	Marks = fields:find(Fs, marks),
	BookMark = case is_question_bookmarked() of
		true -> "<i class='icon-bookmark'></i>";
		false -> []
	end,
	Reported = case is_question_reported() of
		true -> "<i class='icon-warning-sign'></i>";
		false -> []
	end,
	#panel { body=[
		#span {class="mylabel label label-default", text=io_lib:format(locale:get(exam_question_info_index), [Index])},
		#span {class="mylabel label label-default", text=io_lib:format(locale:get(exam_question_info_marks), [Marks#field.uivalue])},
		#span {class="mylabel label label-default", body=BookMark},
		#span {class="mylabel label label-important", body=Reported}
	]}.

%---------------------------------------------------------------------------------------------------
layout_question_desc(Fs) ->
	Description = fields:find(Fs, questiondescription),
	#panel {class="well", body=Description#field.uivalue}.

%---------------------------------------------------------------------------------------------------
layout_question_options(Fs) ->
	Id = fields:find(Fs, '_id'),
	A = fields:find(Fs, optiona),
	B = fields:find(Fs, optionb),
	C = fields:find(Fs, optionc),
	D = fields:find(Fs, optiond),
	#radiogroup {class="exam_question_options", body=[
		#table {class="table table-hover", rows=[
			#tablerow {cells=layout_option_header()},
			#tablerow {cells=layout_option(Id, A)},
			#tablerow {cells=layout_option(Id, B)},
			#tablerow {cells=layout_option(Id, C)},
			#tablerow {cells=layout_option(Id, D)}
		]}
	]}.

layout_option_header() -> [
	#tableheader {body=locale:get(exam_option_header)},
	#tableheader {body=locale:get(exam_option_description)}
].

layout_option(_, undefined) ->
	[];
layout_option(Id, O) ->
	IsChecked = (getanswered(Id#field.uivalue) == O#field.id), [
	#tablecell {body=[
		#radio {name=myradio, checked=IsChecked, text=locale:get(O#field.id), postback={option, Id#field.uivalue, O#field.id}}
	]},
	#tablecell {body=[
		#span {body=O#field.uivalue}
	]}
].

%---------------------------------------------------------------------------------------------------
layout_submit_confirm() ->
	#panel {class="myconfirmpanel", body=[
		#span {class="label label-important", text=locale:get(exam_submit_important)},
		#hr{},
		#span {text=locale:get(exam_submit_test_confirm)},
		#hr{},
		#button {class="btn btn-danger", text=locale:get(exam_submit_test_confirm_yes), postback={submit_test, yes}},
		#button {class="btn btn-default", text=locale:get(exam_submit_test_confirm_no), postback={submit_test, no}}
	]}.

%---------------------------------------------------------------------------------------------------
% NAVIGATION
%---------------------------------------------------------------------------------------------------
nav_left() ->
	#panel { body=[
		#panel {body=getuservalue(oeuserfullname)},
		#panel {body="[" ++ getuservalue(oeusercentercode) ++ ", " ++ getuservalue('_id') ++ "]"}
	]}.
nav_right() ->
	#panel { body=[
		#panel {body=gettestvalue(testname)},
		#panel {class="pull-right", id=exam_timer, body=[]}
	]}.

actions_left() ->
	#panel {class="actions_left", body=[
		#button {class="btn btn-info", text=locale:get(exam_questions_list), postback=exam_questions_list},
		#button {class="btn btn-default", text=locale:get(exam_marker), postback=exam_marker},
		#button {class="btn btn-default", text=locale:get(exam_report_invalid), postback=exam_report_invalid}
	]}.
actions_center() ->
	#panel {class="actions_center", body=[
		#link {class="exam_nav_link", text=locale:get(exam_first), postback=exam_first},
		#link {class="exam_nav_link", text=locale:get(exam_previous), postback=exam_previous},
		#link {class="exam_nav_link", text=locale:get(exam_next), postback=exam_next},
		#link {class="exam_nav_link", text=locale:get(exam_last), postback=exam_last}
	]}.
actions_right() ->
	#panel {class="actions_right", body=[
		#button {class="btn btn-default", text=locale:get(exam_clear_selection), postback=exam_clear_selection},
		#button {class="btn btn-danger", text=locale:get(exam_submit_test), postback=exam_submit_test}
	]}.

%---------------------------------------------------------------------------------------------------
% EVENTS
%---------------------------------------------------------------------------------------------------
event({timer_flash, _} = E) ->
	helper_ui:event(E);

event({timer_exam, TimeLeft}) ->
	loop_timer(TimeLeft);

event(exam_questions_list) ->
	wf:update(exam_page, layout_question_list());

event(exam_marker) ->
	on_save_marker(save_marker());

event(exam_report_invalid) ->
	on_save_reported(save_reported());

event(exam_first) ->
	helper:state(questionindex, 1),
	updatequestion();

event(exam_previous) ->
	Index = helper:state(questionindex),
	if
		Index > 1 -> helper:state(questionindex, Index - 1);
		true -> ok
	end,
	updatequestion();

event(exam_next) ->
	Index = helper:state(questionindex),
	Length = length(getuservalue(oeuserqna)),
	if
		Index < Length -> helper:state(questionindex, Index + 1);
		true -> ok
	end,
	updatequestion();

event(exam_last) ->
	helper:state(questionindex, length(getuservalue(oeuserqna))),
	updatequestion();

event(exam_clear_selection) ->
	on_clear_option(clear_option());

event(exam_submit_test) ->
	wf:update(exam_page, layout_submit_confirm());

event({option, Id, Option}) ->
	on_save_option(save_option(Id, Option));

event({question_list, Index}) ->
	helper:state(questionindex, Index),
	updatequestion();

event({submit_test, no}) ->
	updatequestion();

event({submit_test, yes}) ->
	save_timer(0),
	helper:redirect("/login");

event(Event) ->
	helper:print(Event).

%---------------------------------------------------------------------------------------------------
% HELPERS
%---------------------------------------------------------------------------------------------------
getuservalue(Type) ->
	Fs = myauth:userfields(),
	F = fields:find(Fs, Type),
	F#field.uivalue.

gettestvalue(Type) ->
	Fs = myauth:testfields(),
	F = fields:find(Fs, Type),
	F#field.uivalue.

getquestionfields(Id) ->
	questions:get(?DB_QUESTIONS ++ gettestvalue('_id'), Id).

getanswered(Id) ->
	QnA = getuservalue(oeuserqna),
	case lists:keyfind(Id, 1, QnA) of
		{Id, "a"} -> optiona;
		{Id, "b"} -> optionb;
		{Id, "c"} -> optionc;
		{Id, "d"} -> optiond;
		{Id, "e"} -> optione;
		_ -> undefined
	end.

updatequestion() ->
	wf:update(exam_page, layout_question()),
	wf:wire("MathJax.Hub.Queue([\"Typeset\",MathJax.Hub]);").

getoptionvalue(optiona) -> "a";
getoptionvalue(optionb) -> "b";
getoptionvalue(optionc) -> "c";
getoptionvalue(optiond) -> "d";
getoptionvalue(optione) -> "e";
getoptionvalue(_) -> "0".

getoptiondisplay("a") -> "A";
getoptiondisplay("b") -> "B";
getoptiondisplay("c") -> "C";
getoptiondisplay("d") -> "D";
getoptiondisplay("e") -> "E";
getoptiondisplay(_) -> "".

is_question_reported() ->
	is_question_reported(helper:state(questionindex)).
is_question_reported(Index) ->
	Reported = getuservalue(oeuserreported),
	case lists:keyfind(qid(Index), 1, Reported) of
		{_, "true"} -> true;
		_ -> false
	end.

is_question_bookmarked() ->
	is_question_bookmarked(helper:state(questionindex)).
is_question_bookmarked(Index) ->
	Markers = getuservalue(oeusermarkers),
	case lists:keyfind(qid(Index), 1, Markers) of
		{_, "true"} -> true;
		_ -> false
	end.

qid() ->
	qid(helper:state(questionindex)).
qid(Index) ->
	{Id, _} = lists:nth(Index, getuservalue(oeuserqna)),
	Id.

%---------------------------------------------------------------------------------------------------
% SAVE TO DB
%---------------------------------------------------------------------------------------------------
save_option(QuestionId, OptionId) ->
	Fs = myauth:userfields(),
	FQnA = fields:find(Fs, oeuserqna),
	NewQnA = lists:keystore(QuestionId, 1, FQnA#field.uivalue, {QuestionId, getoptionvalue(OptionId)}),
	NewFs = fields:delete(Fs, oeuserqna) ++ [FQnA#field {uivalue=NewQnA}],
	save_user(NewFs).

clear_option() ->
	case lists:nth(helper:state(questionindex), getuservalue(oeuserqna)) of
		{_, "0"} -> {noop, "0"};
		{QId, _} -> save_option(QId, "0")
	end.

save_marker() ->
	QuestionId = qid(),
	Fs = myauth:userfields(),
	FMarkers = fields:find(Fs, oeusermarkers),
	Marker = helper:a2l(not is_question_bookmarked()),
	NewMarkers = lists:keystore(QuestionId, 1, FMarkers#field.uivalue, {QuestionId, Marker}),
	NewFs = fields:delete(Fs, oeusermarkers) ++ [FMarkers#field {uivalue=NewMarkers}],
	save_user(NewFs).

save_reported() ->
	QuestionId = qid(),
	Fs = myauth:userfields(),
	FReported = fields:find(Fs, oeuserreported),
	Reported = helper:a2l(not is_question_reported()),
	NewReported = lists:keystore(QuestionId, 1, FReported#field.uivalue, {QuestionId, Reported}),
	NewFs = fields:delete(Fs, oeuserreported) ++ [FReported#field {uivalue=NewReported}],
	save_user(NewFs).

save_timer(TimeLeftSeconds) when TimeLeftSeconds < 0 ->
	save_timer(0);
save_timer(TimeLeftSeconds) ->
	Fs = myauth:userfields(),
	TimeLeft = fields:find(Fs, oeusertimeleftseconds),
	on_save_timer(save_user(fields:delete(Fs, oeusertimeleftseconds) ++ [TimeLeft#field {uivalue=helper:i2s(TimeLeftSeconds)}])).

save_user(Fs) ->
	TestId = gettestvalue('_id'),
	oeusers:update(?DB_USERS ++ TestId, Fs).

%---------------------------------------------------------------------------------------------------
% ON SAVE
%---------------------------------------------------------------------------------------------------
on_save_option({ok, Doc}) ->
	myauth:userfields(helper_api:doc2fields({ok, Doc})),
	{_, O} = lists:nth(helper:state(questionindex), getuservalue(oeuserqna)),
	helper_ui:flash(success, io_lib:format(locale:get(exam_option_save_success), [getoptiondisplay(O), helper:state(questionindex)]));
on_save_option(_) ->
	helper_ui:flash(error, io_lib:format(locale:get(exam_option_save_failed), [helper:state(questionindex)])).

on_save_marker({ok, Doc}) ->
	myauth:userfields(helper_api:doc2fields({ok, Doc})),
	updatequestion();
on_save_marker(_) ->
	helper_ui:flash(error, io_lib:format(locale:get(exam_marker_save_failed), [helper:state(questionindex)])).

on_save_reported({ok, Doc}) ->
	myauth:userfields(helper_api:doc2fields({ok, Doc})),
	updatequestion();
on_save_reported(_) ->
	helper_ui:flash(error, io_lib:format(locale:get(exam_reported_save_failed), [helper:state(questionindex)])).

on_clear_option({noop, _}) ->
	ok;
on_clear_option({ok, Doc}) ->
	myauth:userfields(helper_api:doc2fields({ok, Doc})),
	updatequestion(),
	helper_ui:flash(warning, io_lib:format(locale:get(exam_option_clear_success), [helper:state(questionindex)]));
on_clear_option(_) ->
	helper_ui:flash(error, io_lib:format(locale:get(exam_option_clear_failed), [helper:state(questionindex)])).

on_save_timer({ok, Doc}) ->
	myauth:userfields(helper_api:doc2fields({ok, Doc}));
on_save_timer(_) ->
	helper_ui:flash(exam_timer_save_failed).

%---------------------------------------------------------------------------------------------------
% END
%---------------------------------------------------------------------------------------------------
-module (exam).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

-define(INTERVAL_SECS_COMET, 20).

%---------------------------------------------------------------------------------------------------
% INIT MODULE
%---------------------------------------------------------------------------------------------------
main() ->
	handlePageReload(myauth:pageloaded(?MODULE)).

handlePageReload(true) ->
	log(info, handlePageReload, true),
	helper:redirect("/login");
handlePageReload(false) ->
	myauth:main(?MODULE).

title() ->
	locale:get(exam_title).

heading() ->
	locale:get(exam_heading).

brand(customer_image) ->
	Fs = myauth:testfields(),
	case fields:getuivalue(Fs, oecustomerid) of
		[] ->
			configs:get(customer_image);
		Id ->
			lists:flatten(io_lib:format("/images/~s-logo.jpg", [Id]))
	end.

layout() ->
	MyServer = spawn(?MODULE, myserver, [myauth:testfields(), myauth:userfields()]),
	helper:state(myserver, MyServer),
	on_initexam(initexam()),
	myauth:pageloaded(?MODULE, true),
	helper:state(questionindex, 1),
	QuestionElements = try
		layout_question()
	catch
		_:_ -> "Error #0002"
	end,
	Body = #panel {id=exam_page, body=QuestionElements},
	wf:switch_to_polling(?INTERVAL_SECS_COMET*1000),
	wf:comet(fun() -> loop_timer(helper:s2i(getuservalue(oeusertimeleftseconds))) end),
	Body.

%---------------------------------------------------------------------------------------------------
% MYSERVER
%---------------------------------------------------------------------------------------------------
myserver(T, U) ->
	receive
		{Client, save, Tuples} ->
			NU = lists:foldl(fun({K, V}, Acc) ->
				F = fields:get(K),
				fields:delete(Acc, K) ++ [F#field {uivalue=V}]
			end, U, Tuples),
			Response = oeusers:update(?DB_USERS ++ fields:getuivalue(T, '_id'), NU),
			Client ! {response_save, Response},
			case Response of
				{ok, _} -> myserver(T, helper_api:doc2fields(Response));
				_ -> myserver(T, U)
			end;
		{Client, get, Id} ->
			Client ! {response_get, fields:getuivalue(U, Id)},
			myserver(T, U)
	after 10*60*1000 ->
		log:log("myserver, exit, idle for 10 minutes")
	end.

%---------------------------------------------------------------------------------------------------
% TIMERS
%---------------------------------------------------------------------------------------------------
loop_timer(TimeLeft) when TimeLeft < 1 ->
	endexam(0);
loop_timer(TimeLeft) ->
	try
		case myauth:username() of
			undefined ->
				ok;
			_ ->
				log(info, loop_timer, TimeLeft),
				Interval = 60,
				wf:update(exam_timer, layout_timer(TimeLeft)),
				wf:flush(),
				timer:sleep(Interval*1000),
				save_timer(TimeLeft - Interval),
				loop_timer(TimeLeft - Interval)
		end
	catch
		X:Y ->
			log(exception, loop_timer, lists:flatten(io_lib:format("~p ~p ~p", [TimeLeft, X, Y]))),
			helper:redirect("/login")
	end.

%---------------------------------------------------------------------------------------------------
% LAYOUTS
%---------------------------------------------------------------------------------------------------
layout_timer(TimeLeft) ->
	TimeFmt = case TimeLeft < 61 of
		true -> "< 1 min";
		_ -> io_lib:format("~p min", [TimeLeft div 60])
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
	MainText = case fields:find(Fs, maintext) of
		undefined -> [];
		Text -> #panel {class="well", body=Text#field.uivalue}
	end,
	Description = fields:find(Fs, questiondescription),
	[MainText, #panel {class="well", body=Description#field.uivalue}].

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
	MinAppearSeconds = helper:s2i(gettestvalue(oe_min_appear_minutes))*60,
	TimeLeftSeconds = helper:s2i(getuservalue(oeusertimeleftseconds)),
	TestDuration = helper:s2i(gettestvalue(testduration))*60,
	case MinAppearSeconds > (TestDuration - TimeLeftSeconds) of
		true ->
			#panel {class="myconfirmpanel", body=[
				#span {class="label label-warning", text=locale:get(exam_submit_important)},
				#hr{},
				#span {text=io_lib:format(locale:get(exam_submit_test_min_appear), [gettestvalue(oe_min_appear_minutes)])},
				#hr{},
				#button {class="btn btn-default", text=locale:get(exam_submit_test_min_appear_ok), postback={submit_test, no}}
			]};
		false ->
			#panel {class="myconfirmpanel", body=[
				#span {class="label label-important", text=locale:get(exam_submit_important)},
				#hr{},
				#span {text=locale:get(exam_submit_test_confirm)},
				#hr{},
				#button {class="btn btn-danger", text=locale:get(exam_submit_test_confirm_yes), postback={submit_test, yes}},
				#button {class="btn btn-default", text=locale:get(exam_submit_test_confirm_no), postback={submit_test, no}}
			]}
	end.

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
	InvalidElement = case gettestvalue(oe_report_invalid_enabled) of
		"no" -> [];
		_ -> #button {class="btn btn-default", text=locale:get(exam_report_invalid), postback=exam_report_invalid}
	end,
	#panel {class="actions_left", body=[
		#button {class="btn btn-info", text=locale:get(exam_questions_list), postback=exam_questions_list},
		#button {class="btn btn-default", text=locale:get(exam_marker), postback=exam_marker},
		InvalidElement
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
event(Any) ->
	case wf:session_id() == cache:session_id(myauth:username()) of
		true -> myevent(Any);
		false -> helper:redirect("/login")
	end.

myevent(exam_questions_list) ->
	wf:update(exam_page, layout_question_list());

myevent(exam_marker) ->
	on_save_marker(save_marker());

myevent(exam_report_invalid) ->
	on_save_reported(save_reported());

myevent(exam_first) ->
	helper:state(questionindex, 1),
	updatequestion();

myevent(exam_previous) ->
	Index = helper:state(questionindex),
	if
		Index > 1 -> helper:state(questionindex, Index - 1);
		true -> ok
	end,
	updatequestion();

myevent(exam_next) ->
	Index = helper:state(questionindex),
	Length = length(getuservalue(oeuserqna)),
	if
		Index < Length -> helper:state(questionindex, Index + 1);
		true -> ok
	end,
	updatequestion();

myevent(exam_last) ->
	helper:state(questionindex, length(getuservalue(oeuserqna))),
	updatequestion();

myevent(exam_clear_selection) ->
	on_clear_option(clear_option());

myevent(exam_submit_test) ->
	wf:update(exam_page, layout_submit_confirm());

myevent({option, Id, Option}) ->
	on_save_option(save_option(Id, Option));

myevent({question_list, Index}) ->
	helper:state(questionindex, Index),
	updatequestion();

myevent({submit_test, no}) ->
	updatequestion();

myevent({submit_test, yes}) ->
	endexam();

myevent(Event) ->
	helper:print(Event).

%---------------------------------------------------------------------------------------------------
% HELPERS
%---------------------------------------------------------------------------------------------------
initexam() ->
	initexam(getuservalue(oeuserexamstate)).

initexam(?YETTOSTART) ->
	save([
		{oeuserexamstate, "active"},
		{oeuserstarttime, helper:i2s(helper:epochtime())},
		{oeuserlogintimes, increment_logintimes(getuservalue(oeuserlogintimes))},
		{oeuserips, append_ip([])}
	]);
initexam(?RELOGIN) ->
	save([
		{oeuserexamstate, "active"},
		{oeuserlogintimes, increment_logintimes(getuservalue(oeuserlogintimes))},
		{oeuserips, append_ip(getuservalue(oeuserips))}
	]);
initexam(Other) ->
	log(error, initexam, Other),
	helper:redirect("/login").

endexam() ->
	endexam(helper:s2i(getuservalue(oeusertimeleftseconds))).

endexam(TimeLeft) ->
	save([
		{oeuserexamstate, "completed"},
		{oeuserendtime, helper:i2s(helper:epochtime())},
		{oeusertimeleftseconds, helper:i2s(TimeLeft)}
	]),
	log(info, endexam, TimeLeft),
	helper:redirect("/login").

increment_logintimes(Times) -> helper:i2s(helper:s2i(Times) + 1).

append_ip(List) ->	List ++ [{helper:epochtimetostring(helper:epochtime()), myauth:ip()}].

getuservalue(Type) ->
	MyServer = helper:state(myserver),
	case MyServer of
		undefined ->
			Fs = myauth:userfields(),
			fields:getuivalue(Fs, Type);
		_ ->
			MyServer ! {self(), get, Type},
			receive
				{response_get, Response} ->
					Response
			after 10000 ->
				{error, timedout}
			end
	end.

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
	try
		wf:update(exam_page, layout_question()),
		wf:wire("MathJax.Hub.Queue([\"Typeset\",MathJax.Hub]);")
	catch
		_:_ ->
			 wf:update(exam_page, "Error #001")
	end.

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

log(Status, Type, Response) ->
	Log = io_lib:format("~p, ~s, ~p, ~p", [Status, myauth:username(), Type, Response]),
	log:log(Log).
%---------------------------------------------------------------------------------------------------
% SAVE TO DB
%---------------------------------------------------------------------------------------------------
save_option(QuestionId, OptionId) ->
	FQnA = getuservalue(oeuserqna),
	NewQnA = lists:keystore(QuestionId, 1, FQnA, {QuestionId, getoptionvalue(OptionId)}),
	save([{oeuserqna, NewQnA}]).

clear_option() ->
	case lists:nth(helper:state(questionindex), getuservalue(oeuserqna)) of
		{_, "0"} -> {noop, "0"};
		{QId, _} -> save_option(QId, "0")
	end.

save_marker() ->
	QuestionId = qid(),
	FMarkers = getuservalue(oeusermarkers),
	Marker = helper:a2l(not is_question_bookmarked()),
	NewMarkers = lists:keystore(QuestionId, 1, FMarkers, {QuestionId, Marker}),
	save([{oeusermarkers, NewMarkers}]).

save_reported() ->
	QuestionId = qid(),
	FReported = getuservalue(oeuserreported),
	Reported = helper:a2l(not is_question_reported()),
	NewReported = lists:keystore(QuestionId, 1, FReported, {QuestionId, Reported}),
	save([{oeuserreported, NewReported}]).

save_timer(TimeLeftSeconds) when TimeLeftSeconds < 0 ->
	endexam(0);
save_timer(TimeLeftSeconds) ->
	on_save_timer(save([{oeusertimeleftseconds, helper:i2s(TimeLeftSeconds)}])).


save(Tuples) ->
	helper:state(myserver) ! {self(), save, Tuples},
	receive
		{response_save, Response} ->
			Response
	after 10000 ->
		{error, timedout}
	end.

%---------------------------------------------------------------------------------------------------
% ON SAVE
%---------------------------------------------------------------------------------------------------
on_save_option({ok, _}) ->
	{_, O} = lists:nth(helper:state(questionindex), getuservalue(oeuserqna)),
	helper_ui:flash(success, io_lib:format(locale:get(exam_option_save_success), [getoptiondisplay(O), helper:state(questionindex)]));
on_save_option(Res) ->
	log(error, on_save_option, Res),
	helper:redirect("/login").

on_save_marker({ok, _}) ->
	updatequestion();
on_save_marker(Res) ->
	log(error, on_save_marker, Res),
	helper:redirect("/login").

on_save_reported({ok, _}) ->
	updatequestion();
on_save_reported(Res) ->
	log(error, on_save_reported, Res),
	helper:redirect("/login").

on_clear_option({noop, _}) ->
	ok;
on_clear_option({ok, _}) ->
	updatequestion(),
	helper_ui:flash(warning, io_lib:format(locale:get(exam_option_clear_success), [helper:state(questionindex)]));
on_clear_option(Res) ->
	log(error, on_clear_option, Res),
	helper:redirect("/login").

on_save_timer({ok, _}) ->
	ok;
on_save_timer(Res) ->
	log(error, on_save_timer, Res),
	helper:redirect("/login").

on_initexam({ok, _}) ->
	ok;
on_initexam(Res) ->
	log(error, on_initexam, Res),
	helper:redirect("/login").

%---------------------------------------------------------------------------------------------------
% END
%---------------------------------------------------------------------------------------------------

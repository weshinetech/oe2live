-module(layout).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

get(undefined, Fields, Events) ->
	get(?CREATE, Fields, Events);
get(Mode, Fields, Events) ->
	Event = findevent(Mode, Events),
	Layout = {
		getfields(Mode, Event, Fields),
		getevent(Event)
	},
	Layout.

getfield(Mode, Event, #field{renderer=RFn} = F) ->
	getfield(Mode, Event, F, RFn).

getfields(Mode, Event, Fs) ->
	lists:foldl(fun(#field {renderer=RFn} = F, Acc) ->
		Acc ++ [getfield(Mode, Event, F, RFn)]
	end, [], Fs).

getfield(_, _, F, undefined) -> helper:print(F), throw(renderer_undefined);
getfield(Mode, #jevent{id=search}, F, RFn) -> RFn(Mode, #jevent {}, F);
getfield(Mode, #jevent{bind=false}, F, RFn) -> RFn(Mode, #jevent {}, F);
getfield(Mode, Event, F, RFn) -> RFn(Mode, Event, F).

findevent(Mode, Events) ->
	case lists:keyfind(Mode, 3, Events) of
		false -> #jevent {};
		Else -> Else
	end.

getevent(#jevent {id=undefined}) -> [];
getevent(#jevent {} = E) -> [
	#button {id=E#jevent.id, text=E#jevent.label, postback=E#jevent.id}
].

form(oe2form_simple, Module, {Fs, Es}) ->
	#panel {class="oe2form-simple", body=[
		#h4 {text=Module:heading()},
		layout(oe2form_simple, fields, Fs),
		layout(oe2form_simple, events, Es),
		#panel {class="myspinner", body=#spinner{}}
	]};
form(oe2form, Module, {Fs, Es}) ->
	#panel {body=[
		#panel {class="oe2form", body=[
			#h3 {class="oe2form-heading", text=Module:heading()},
			layout(oe2form, fields, Fs),
			layout(oe2form, events, Es),
			#panel {class="myspinner", body=#spinner{}}
		]}
	]};
form(oe2form_horizontal, Module, {Fs, Es}) ->
	#panel {class="span6", body=[
		#panel {class="oe2form-horizontal form-horizontal", body=[
			#panel {class="control-group", body=[
				#span {class="control-label", text=""},
				#panel {class="controls", body=[
					#h3 {class="oe2form-horizontal-heading", text=Module:heading()}
				]}
			]},
			layout(oe2form_horizontal, fields, Fs),
			layout(oe2form_horizontal, events, Es),
			layout(oe2form_horizontal, spinner)
		]}
	]}.

layout(oe2form_simple, fields, Fs) ->
	lists:foldl(fun({_, E}, Acc) ->
		Acc ++ [E]
	end, [], Fs);
layout(oe2form_simple, events, Es) ->
	lists:foldl(fun(E, Acc) ->
		Acc ++ [E#button {class="btn btn-primary"}]
	end, [], Es);

layout(oe2form, fields, Fs) ->
	lists:foldl(fun({_, E}, Acc) ->
		Acc ++ [E]
	end, [], Fs);
layout(oe2form, events, Es) ->
	lists:foldl(fun(E, Acc) ->
		Acc ++ [E#button {class="btn btn-large btn-primary"}]
	end, [], Es);

layout(oe2form_horizontal, fields, Fs) ->
	lists:foldl(fun({L, E}, Acc) ->
		case {L, E} of
			{[], []} -> Acc;
			_ ->
				Acc ++ [
					#panel {class="control-group", body=[
						#span {class="control-label", text=L},
						#panel {class="controls", body=E}
					]}
				]
		end
	end, [], Fs);
layout(oe2form_horizontal, events, Es) ->
	lists:foldl(fun(E, Acc) ->
		Acc ++ [
			#panel {class="control-group", body=[
				#span {class="control-label", text=""},
				#panel {class="controls", body=E#button {class="btn btn-primary"}}
			]}
		]
	end, [], Es).

layout(oe2form_horizontal, spinner) ->
	#panel {class="control-group", body=[
		#span {class="control-label", text=""},
		#panel {class="controls", body=#spinner{}}
	]}.

flash() ->
	#flash {}.

g(1, E) -> #panel {class="span1", body=E};
g(2, E) -> #panel {class="span2", body=E};
g(3, E) -> #panel {class="span3", body=E};
g(4, E) -> #panel {class="span4", body=E};
g(5, E) -> #panel {class="span5", body=E};
g(6, E) -> #panel {class="span6", body=E};
g(7, E) -> #panel {class="span7", body=E};
g(8, E) -> #panel {class="span8", body=E};
g(9, E) -> #panel {class="span9", body=E};
g(10, E) -> #panel {class="span10", body=E};
g(11, E) -> #panel {class="span11", body=E};
g(12, E) -> #panel {class="span12", body=E}.

grow() -> #panel {class="row", body=[]}.

table(List, Fields, Columns) ->
	#table {class="table table-bordered table-hover", rows=table_header(Fields, Columns) ++ table_rows(List, Fields, Columns)}.

table_header(Fields, Columns) ->
	Cells = lists:map(fun(_) ->
		header_cells(Fields)
	end, lists:seq(1, Columns)),
	[#tablerow {cells=Cells}].

header_cells(Fields) ->
	lists:map(fun(Id) ->
		F = fields:get(Id),
		#tableheader {body=#span {text=F#field.label}}
	end, Fields).

table_rows(List, Fields, COLUMNS) ->
	CellElements = lists:map(fun(L) -> row_cells(L, Fields) end, List),
	ListOfCells = [lists:sublist(CellElements, X, COLUMNS) || X <- lists:seq(1, length(CellElements), COLUMNS)],
	lists:map(fun(Cs) ->
		Cs1 = case length(Cs) < COLUMNS of
			true -> Cs ++ filler_cells(COLUMNS - length(Cs));
			_ -> Cs
		end,
		#tablerow {cells=Cs1}
	end, ListOfCells).

row_cells(L, Fields) ->
	lists:map(fun(F) -> cell(L, F) end, Fields).

cell(L, Field) ->
	case fields:find(L, Field) of
		undefined -> #tablecell {body=#span {text=""}};
		F -> 
			{_, FE} = getfield(?VIEW, #jevent{}, F),
			#tablecell {body=FE}
	end.

filler_cells(N) ->
	lists:map(fun(_) -> #tablecell {body=""} end, lists:seq(1, N)).

table_fields(Fields, Ids, Rows) ->
	#table {
		class="table table-bordered table-hover",
		rows=lists:map(fun(I) -> 
			#tablerow {cells=table_fields_row(Fields, I)}
		end, Ids) ++ Rows
	}.

table_fields(Fields, Ids) ->
	#table {
		class="table table-bordered table-hover",
		rows=lists:map(fun(I) -> 
			#tablerow {cells=table_fields_row(Fields, I)}
		end, Ids)
	}.

table_fields_row(Fields, I) ->
	F = fields:find(Fields, I),
	table_fields_row_1(F).

table_fields_row_1(undefined) -> [];
table_fields_row_1(F) ->
	{L, FE} = getfield(?VIEW, #jevent{}, F),
	#tablerow { cells=[
		#tablecell {body=#span {text=L}},
		#tablecell {body=FE}
	]}.

table_values([], _) ->
	[];
table_values(Fields, Id) ->
	#table {
		class="table table-bordered table-hover",
		rows=lists:map(fun(F) -> 
			Value = fields:find(F,Id),
			#tablerow {cells=[
				#tablecell {body=[
					#span {text=Value#field.uivalue}
				]}
			]}
		end, Fields)
	}.

row(E) -> #panel {class="row-fluid", body=E}.


table_s(List, Fields, Columns, DefaultTxt) ->
	Header = table_header_s(Fields, Columns),
	Rows = table_rows_s(List, Fields, Columns, DefaultTxt),
	io_lib:format("<table border=\"1\" cellspacing=\"0\" cellpadding=\"5\">~s~s</table>", [Header, Rows]).

table_header_s(Fields, Columns) ->
	Cells = lists:map(fun(_) ->
		header_cells_s(Fields)
	end, lists:seq(1, Columns)),
	io_lib:format("<tr>~s</tr>", [Cells]).

header_cells_s(Fields) ->
	lists:map(fun(Id) ->
		F = fields:get(Id),
		io_lib:format("<td>~s</td>", [F#field.label])
	end, Fields).

table_rows_s(List, Fields, COLUMNS, DefaultTxt) ->
	CellElements = lists:map(fun(L) -> row_cells_s(L, Fields, DefaultTxt) end, List),
	ListOfCells = [lists:sublist(CellElements, X, COLUMNS) || X <- lists:seq(1, length(CellElements), COLUMNS)],
	lists:map(fun(Cs) ->
		Cs1 = case length(Cs) < COLUMNS of
			true -> Cs ++ filler_cells_s(COLUMNS - length(Cs));
			_ -> Cs
		end,
		io_lib:format("<tr>~s</tr>", [Cs1])
	end, ListOfCells).

row_cells_s(L, Fields, DefaultTxt) ->
	lists:map(fun(F) -> cell_s(L, F, DefaultTxt) end, Fields).

cell_s(L, Field, DefaultTxt) ->
	case fields:find(L, Field) of
		undefined -> io_lib:format("<td>~s</td>", [DefaultTxt]);
		F -> io_lib:format("<td>~s</td>", [F#field.uivalue])
	end.

filler_cells_s(N) ->
	lists:map(fun(_) -> "<td>...</td>" end, lists:seq(1, N)).
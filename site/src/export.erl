-module (export).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	case myauth:is_authenticated() of
		false -> 
			helper:redirect("/login");
		_ -> 
			export()
	end.

export() ->
	try
		Results = [],
		File = lists:foldl(fun(FL, Acc) ->
			Acc ++ [getline(FL)]
		end, [], Results),
		wf:content_type("text/csv"),
		Fname = helper:date_d2s(helper:date_today()) ++ "-" ++ helper:time_t2s(helper:time_now()),
		wf:header("Content-Disposition", "filename=" ++ "export-" ++ Fname ++ ".csv"),
		string:join(File, "\n")
	catch
		_:_ ->
			helper:redirect("/login")
	end.

getline(FL) ->
	Line = lists:foldl(fun(F, Acc) ->
		F1 = fields:find(FL, F#field.id),
		Acc ++ [helper:ifundefempty(F1#field.uivalue)] 
	end, [], FL),
	string:join(Line, ",").

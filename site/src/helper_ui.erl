-module(helper_ui).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

%-----------------------------------------------------------------------------------------------
% FLASH
%-----------------------------------------------------------------------------------------------
flash({ok, _}) ->
	flash(success, locale:get(success));
flash({error, _}) ->
	flash(error, locale:get(error));
flash(Other) ->
	flash(warning, Other).

flash(Type, Msg) ->
	TimeOutSeconds = 5,
	Id = helper:uid(),
	wf:update(myalert, getflashelement(Id, Type, Msg)),
	wf:wire(#event{type=timer, delay=TimeOutSeconds*1000, target=Id, actions=#hide{}}).

getflashelement(Id, error, Text) ->
	#panel {id=Id, class="alert alert-error", body=Text};
getflashelement(Id, success, Text) ->
	#panel {id=Id, class="alert alert-success", body=Text};
getflashelement(Id, info, Text) ->
	#panel {id=Id, class="alert alert-info", body=Text};
getflashelement(Id, _, Text) ->
	#panel {id=Id, class="alert", body=Text}.

flash_element() ->
	#panel {id=myalert, class="myalert", body=[]}.

%-----------------------------------------------------------------------------------------------
% HELPERS
%-----------------------------------------------------------------------------------------------
fields(Md) ->
	fields(Md, []).

fields(Md, {error, _}) ->
	fields(Md, []);
fields(Md, Fields) ->
	Id = try
		Md:id()
	catch
		_:_ -> undefined
	end,
	FieldIds = case wf:q(mode) of
		?SEARCH -> Md:sids();
		_ -> Md:fids()
	end,
	FilteredFields = lists:foldl(fun(FId, Acc) ->
		F = fields:get(FId),
		FNew = case F#field.id == Id of
			true -> getfieldpk(F, fields:filter(Fields, '_id'));
			false -> getfield(F, fields:filter(Fields, FId))
		end,
		Acc ++ fields:toui([FNew])
	end, [], FieldIds),
	FilteredFields ++ fieldsFK(Md, Fields).

getfieldpk(F, []) -> F#field {type=primarykey};
getfieldpk(F, [IdField]) -> F#field {type=primarykey, dbvalue=IdField#field.dbvalue}.

getfield(F, []) -> F;
getfield(_, [F]) -> F.

fieldsFK(Md, Fields) ->
	try
		lists:foldl(fun(FId, Acc) ->
			F = case fields:filter(Fields, FId) of
				[FDb] -> FDb;
				_ -> fields:get(FId)
			end,
			Acc ++ fields:toui([F#field {type=foreignkey}])
		end, [], Md:fkids())
	catch
		_:_ -> []
	end.

events(EIds) ->
	lists:foldl(fun(EId, Acc) ->
		Acc ++ [events:get(EId)]
	end, [], EIds).

tmpid(Module, Id) ->
	helper:l2a(helper:a2l(Module) ++ Id).

space() -> "<pre> </pre>".

oe2version() ->
	"Version: " ++ configs:get(oe2version).

print_button() ->
	"<button class='btn btn-primary hidden-print mylabel' onclick='window.print();'>Print</button>".

download_button() ->
	#button {class="btn btn-primary hidden-print mylabel", postback=download, text=locale:get(msg_download)}.

%-----------------------------------------------------------------------------------------------
% FULL PAGE
%-----------------------------------------------------------------------------------------------
fullpage(E) ->
	Module = wf:page_module(),
	Heading = case wf:q(mode) of
		undefined -> Module:heading();
		Mode -> io_lib:format("~s (~s)", [Module:heading(), string:to_upper(Mode)])
	end,
	[#h4 {text=Heading}, #hr {}, E].
%-----------------------------------------------------------------------------------------------
% END
%-----------------------------------------------------------------------------------------------
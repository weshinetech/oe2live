-module(helper_ui).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

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
	wf:wire(#event{type=timer, delay=TimeOutSeconds*1000, postback={timer_flash, Id}}).

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

event({timer_flash, Id}) ->
	wf:remove(Id).

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
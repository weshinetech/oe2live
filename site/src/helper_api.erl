-module(helper_api).
-compile(export_all).
-include_lib("records.hrl").

fields2doc(Fields) ->
	Doc = lists:foldl(fun(#field {subfields=Sf, type=T, uivalue=V, dbvalue=D} = F, Acc) ->
		case Sf of
			undefined -> 
				DbValue = case fields:todb(T, V) of
					undefined -> D;
					NewD -> NewD
				end,
				Acc ++ [{helper:a2dbid(helper:id(F)), DbValue}];
			_ ->
				Acc ++ [{helper:a2dbid(helper:id(F)), fields2doc(Sf)}]
		end
	end, [], Fields),
	{Doc}.

doc2fields({ok, Doc}) ->
	doc2fields(undefined, Doc);
doc2fields(Other) ->
	Other.

doc2fields(ParentId, {Doc}) ->
	lists:foldl(fun({Id, Value}, Acc) ->
		F = fields:get(helper:dbid2a(Id)),
		if
			F == undefined ->
				Acc;
			F#field.subfields == undefined ->
				Acc ++ [F#field {baseid=F#field.id, id=helper:id(ParentId, F#field.id),
					dbvalue=Value}];
			true ->
				Acc ++ case Value of
					{_} -> [F#field {subfields=doc2fields(F#field.id, Value)}];
					_ -> []
				end
		end
	end, [], Doc).

response({ok, {Doc}}) ->
	Id = proplists:get_value(<<"_id">>, Doc),
	{ok, helper:b2l(Id)};
response(Other) ->
	Other.

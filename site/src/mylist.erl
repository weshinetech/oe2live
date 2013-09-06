-module(mylist).
-compile(export_all).
-include_lib("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

get(I) ->
	?MODULE:get(mylist, I, locale:get(I)).

get(mylist, Id, Label) ->
	#field {
		id=Id,
		type=mylist,
		label=Label,
		renderer=renderer(),
		validators=[]
	}.

renderer() ->
	fun(Md, _, #field {id=I, uivalue=V}) ->
		if
			Md == ?CREATE ->
				#hidden {id=I, text=[]};
			Md == ?EDIT ->
				helper:state(I, V),
				#hidden {id=I, text=[]};
			true ->
				[]
		end
	end.

toui(undefined) ->
	undefined;
toui(List) -> 
	lists:foldl(fun(I, Acc) ->
		Acc ++ [helper:b2l(I)]
	end, [], List).

uivalue(I) ->
	case helper:state(I) of
		undefined -> [];
		V -> V
	end.

todb(List) ->
	lists:foldl(fun(I, Acc) ->
		Acc ++ [helper:l2b(I)]
	end, [], List).

% api
add(Field, ToAdd) ->
	List = Field#field.dbvalue,
	NewList = case lists:member(ToAdd, List) of
		true -> List;
		false -> List ++ [ToAdd]
	end,
	Field#field {dbvalue=NewList}.

remove(Field, ToRemove) ->
	List = Field#field.dbvalue,
	NewList = case lists:member(ToRemove, List) of
		true -> lists:delete(ToRemove, List);
		false -> List
	end,
	Field#field {dbvalue=NewList}.

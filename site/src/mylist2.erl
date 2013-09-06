-module(mylist2).
-compile(export_all).
-include_lib("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

get(I) ->
	?MODULE:get(mylist2, I, locale:get(I)).

get(mylist2, Id, Label) ->
	#field {
		id=Id,
		type=mylist2,
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
	lists:foldl(fun({[{K, V}]}, Acc) ->
		Acc ++ [{helper:b2l(K), helper:b2l(V)}]
	end, [], List).

uivalue(I) ->
	case helper:state(I) of
		undefined -> [];
		V -> V
	end.

todb(List) ->
	lists:foldl(fun({K, V}, Acc) ->
		Acc ++ [{[{helper:l2b(K), helper:l2b(V)}]}]
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

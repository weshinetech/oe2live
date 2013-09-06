-module(questions).
-compile(export_all).
-include_lib("records.hrl").

get(Db, Id) ->
	helper_api:doc2fields(db:get(Db, Id)).

getquestions(TestId) ->
	Docs = db:getdocs(?DB_QUESTIONS ++ TestId),
	Questions = lists:map(fun(D) -> helper_api:doc2fields({ok, D}) end, Docs),
	Questions.

getdict(TestId) ->
	Questions = ?MODULE:getquestions(TestId),
	List = lists:map(fun(Q) ->
		Id = fields:find(Q, '_id'),
		{Id#field.uivalue, Q}
	end, Questions),
	dict:from_list(List).
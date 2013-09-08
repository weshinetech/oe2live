-module(oeusers).
-compile(export_all).
-include_lib("records.hrl").

get(Db, Id) ->
	helper_api:doc2fields(db:get(Db, Id)).

create(Db, Fields) ->
	db:save(Db, helper_api:fields2doc(Fields)).

createall(Db, ListOfFields) ->
	Docs = lists:map(fun(Fs) ->
		helper_api:fields2doc(Fs)
	end, ListOfFields),
	db:savebulk(Db, Docs).

update(Db, Fields) ->
	db:save(Db, helper_api:fields2doc(Fields)).

updateall(Db, ListOfFields) ->
	Docs = lists:map(fun(Fs) ->
		helper_api:fields2doc(Fs)
	end, ListOfFields),
	db:savebulk(Db, Docs).

delete(Db, Id) ->
	{ok, Doc} = db:get(Db, Id),
	db:delete(Db, Doc).

deleteall(Db) ->
	db:deleteall(Db).

getuser(undefined, _) -> [];
getuser(TestId, UserName) ->
	get(?DB_USERS ++ TestId, UserName).

getusers(TestId) ->
	Docs = db:getdocs(?DB_USERS ++ TestId),
	Users = lists:map(fun(D) -> helper_api:doc2fields({ok, D}) end, Docs),
	Users.

getusers_by_state(TestId) ->
	Users = oeusers:getusers(TestId),
	{Y, A, C} = lists:foldl(fun(U, {AccY, AccA, AccC}) ->
		#field {uivalue=State} = fields:find(U, oeuserexamstate),
		case State of
			?COMPLETED -> {AccY, AccA, AccC ++ [U]};
			?YETTOSTART -> {AccY ++ [U], AccA, AccC};
			?ACTIVE -> {AccY, AccA ++ [U], AccC}
		end
	end, {[], [], []}, Users),
	{Y, A, C}.

compute_scores(TestId) ->
	{_, _, Users} = getusers_by_state(TestId),
	Dict = questions:getdict(TestId),
	NewUsers = lists:map(fun(U) -> compute_score(Dict, U) end, Users),
	?MODULE:updateall(?DB_USERS ++ TestId, NewUsers).

compute_score(Dict, User) ->
	Answers = fields:find(User, oeuserqna),
	ScoreList = lists:map(fun({Q, A}) ->
		compute_score_question(Dict, Q, A)
	end, Answers#field.uivalue),
	Score = lists:foldl(fun(S, Acc) ->
		Acc + helper:s2n(S)
	end, 0, ScoreList),
	FScore = fields:get(oeuserscore),
	NewUser = fields:delete(User, oeuserscore) ++ [FScore#field {uivalue=helper:n2s(Score)}],
	NewUser.

compute_score_question(Dict, Q, A) ->
	case dict:find(Q, Dict) of
		{ok, Fields} -> getscore(Fields, A);
		_ -> "0"
	end.

getscore(Fields, A) ->
	Answer = fields:find(Fields, answer),
	Marks = fields:find(Fields, marks),
	case string:to_lower(Answer#field.uivalue) == string:to_lower(A) of
		true -> Marks#field.uivalue;
		false -> "0"
	end.
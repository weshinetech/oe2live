-module(profiles).
-compile(export_all).
-include_lib("records.hrl").

getdb() ->
	"profiles".

get(Id) ->
	helper_api:doc2fields(db:get(getdb(), Id)).

create(Fields) ->
	Res = db:save(getdb(), helper_api:fields2doc(Fields)),
	oncreate(Res, Fields),
	Res.

oncreate(_, _) ->
	ok.

update(Fields) ->
	Res = db:save(getdb(), helper_api:fields2doc(Fields)),
	onupdate(Res, Fields),
	Res.

onupdate(_, _) ->
	ok.

getuser(Username) ->
	Users = lists:map(fun(D) ->
		helper_api:doc2fields({ok, D})
	end, db:getdocs(getdb())),
	List = lists:filter(fun(F) -> 
		UsernameF = fields:find(F, username), 
		UsernameF#field.uivalue == Username
	end, Users),
	case List of
		[User] -> User;
		_ -> {error, not_found}
	end.
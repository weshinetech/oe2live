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
		case UsernameF = fields:find(F, username) of
			undefined -> false;
			_ -> UsernameF#field.uivalue == Username
		end
	end, Users),
	case List of
		[User] -> User;
		_ -> {error, not_found}
	end.

-module(db).
-compile(export_all).

host() ->
	configs:get(db_host).

port() ->
	configs:get(db_port).

user() ->
	configs:get(db_user).

password() ->
	helper:dbra(configs:get(db_password)).

connection() ->
	Host = host(),
	Port = port(),
	Prefix = "",
	User = user(),
	Password = password(),
	Options = [{basic_auth, {User, Password}}],
	couchbeam:server_connection(Host, Port, Prefix, Options).

db(DbName) ->
	case couchbeam:open_db(connection(), DbName, []) of
		{ok, DB} -> DB;
		_ -> throw(connection_failed)
	end.

save(DbName, Doc) ->
	case couchbeam:save_doc(db(DbName), Doc) of
		{ok, SavedDoc} -> {ok, SavedDoc};
		ErrResponse -> {error, ErrResponse}
	end.

savebulk(DbName, Docs) ->
	case couchbeam:save_docs(db(DbName), Docs) of
		{ok, SavedDocs} -> {ok, SavedDocs};
		ErrResponse -> {error, ErrResponse}
	end.

get(_, []) ->
	{error, id_undefined};
get(_, undefined) ->
	{error, id_undefined};
get(DbName, Id) ->
	case couchbeam:open_doc(db(DbName), Id) of
		{ok, Doc} -> {ok, Doc};
		ErrResponse -> {error, ErrResponse}
	end.

getdocs(DbName) ->
	case couchbeam_view:all(db(DbName), [include_docs]) of
		{ok, Docs} ->
			lists:foldl(fun({Doc}, Acc) ->
				case helper:b2l(couchbeam_util:get_value(<<"id">>, Doc)) of
					"_design/" ++ _ -> Acc;
					_ -> Acc ++ [couchbeam_util:get_value(<<"doc">>, Doc)]
				end
			end, [], Docs);
		_ -> []
	end.

delete(DbName, Doc) ->
	case couchbeam:delete_doc(db(DbName), Doc) of
		{ok, Result} -> {ok, Result};
		_ -> {error, delete_failed}
	end.

-module(utils_oe).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(utils_oe).

heading() ->
	locale:get(utils_oe).

layout() -> 
	[].

% data
admin(undefined) -> undefined;
admin(Cd) ->
	Id = "org.couchdb.user:" ++ Cd ++ "admin",
	case doc(ip(), un(), pw(), "_users", Id) of 
		{ok, {Doc}} -> [
			b2l(couchbeam_util:get_value(<<"college_code">>, Doc)),
			b2l(couchbeam_util:get_value(<<"college_name">>, Doc)),
			b2l(couchbeam_util:get_value(<<"contact_no">>, Doc)),
			b2l(couchbeam_util:get_value(<<"college_contact">>, Doc)),
			b2l(couchbeam_util:get_value(<<"full_name">>, Doc)),
			b2l(couchbeam_util:get_value(<<"server_ip">>, Doc))
		];
		_ -> undefined
	end.

admins() ->
	viewof(ip(), un(), pw(), "_users", "um_d", "v_admins").

tests(Ip, Un, Pw) ->
	viewof(Ip, Un, Pw, "tm", "tm_d", "v_tests_course").

ctests(Ip) ->
	viewof(Ip, cun(), cpw(), "tm", "tm_d", "v_tests_course").

viewof(Ip, Un, Pw, DbName, DbDoc, DbView) ->
	case couchbeam_view:fetch(db(Ip, Un, Pw, DbName), {DbDoc, DbView}, [{reduce, false}, {include_docs, true}]) of
		{ok, List} -> 
			lists:foldl(fun({[{<<"id">>, Id}, _K, {_, V}]}, Acc) ->
				Acc ++ [[b2l(Id)] ++ lists:foldl(fun(Vi, AccV) ->
					AccV ++ [b2l(Vi)]
				end, [], V)]
			end, [], List);
		_ -> []
	end.

doc(Ip, Un, Pw, DbName, DId) ->
	couchbeam:open_doc(db(Ip, Un, Pw, DbName), DId). 
	
% setup
setup(Cd, Ip) ->
	try
		ok = setup_app(Ip),
		ok = setup_data(Cd, Ip),
		ok
	catch
		X:Y -> {error, setup, X, Y}
	end.

setup_app(Ip) ->
	try
		ok = is_up(Ip),
		ok = create_dbs(Ip),
		ok = security_dbs(Ip),
		ok = push_dbs(Ip),
		ok
	catch
		X:Y -> {error, setup_app, X, Y}
	end.

setup_data(Cd, Ip) ->
	push_datas(Cd, Ip).

nuke(Ip) ->
	delete_dbs(Ip).

% helpers

% batch
create_dbs(Ip) ->
	try
		lists:foreach(fun(D) -> ok = create_db(Ip, D) end, dbs())
	catch
		X:Y -> {error, X, Y}
	end.

delete_dbs(Ip) ->
	try
		lists:foreach(fun(D) -> ok = delete_db(Ip, D) end, dbs())
	catch
		X:Y -> {error, X, Y}
	end.

security_dbs(Ip) ->
	try
		lists:foreach(fun(D) -> ok = security_db(Ip, D) end, dbs())
	catch
		X:Y -> {error, X, Y}
	end.

push_dbs(Ip) ->
	try
		lists:foreach(fun(D) -> ok = push_db(Ip, D) end, dbs())
	catch
		X:Y -> {error, X, Y}
	end.

push_datas(Cd, Ip) ->
	try
		lists:foreach(fun(D) -> ok = push_data(Cd, Ip, D) end, data_dbs())
	catch
		X:Y -> {error, X, Y}
	end.

% single
create_db(Ip, Db) ->
	Url = root_college(Ip) ++ "/" ++ Db,
	case ibrowse:send_req(Url, [{"Content-Type", "application/json"}], put, Db) of
		{ok, "20" ++ _, _, _} -> ok;
		{ok, "412", _, _} -> ok;
		_ -> {error, create_db, Ip, Db}
	end.

delete_db(Ip, Db) ->
	Url = root_college(Ip) ++ "/" ++ Db,
	case ibrowse:send_req(Url, [{"Content-Type", "application/json"}], delete, Db) of
		{ok, "20" ++ _, _, _} -> ok;
		{ok, "404", _, _} -> ok;
		_ -> {error, delete_db, Ip, Db}
	end.

security_db(Ip, Db) ->
	Url = root_college(Ip) ++ "/" ++ Db ++ "/_security",
	case ibrowse:send_req(Url, [{"Content-Type", "application/json"}], put, sec(Db)) of
		{ok, "20" ++ _, _, _} -> ok;
		_ -> {error, security_db, Ip, Db}
	end.

push_db(ToIp, Db) ->
	Url = root_uop() ++ "/_replicate",
	case ibrowse:send_req(Url, [{"Content-Type", "application/json"}], post, rep(ToIp, Db), [{inactivity_timeout, 600 * 1000}]) of
		{ok, "20" ++ _, _, _} -> ok;
		_ -> {error, push_db, ToIp, Db}
	end.

push_data(Cd, ToIp, Db) ->
	Url = root_uop() ++ "/_replicate",
	case ibrowse:send_req(Url, [{"Content-Type", "application/json"}], post, rep_data(Cd, ToIp, Db), [{inactivity_timeout, 600 * 3* 1000}]) of
		{ok, "20" ++ _, _, _} -> ok;
		_ -> {error, push_data, Cd, ToIp, Db}
	end.

is_up(Ip) ->
	Url = "http://" ++ Ip ++ ":5984",
	case req_get(Url) of
		{ok, "200", _, _} -> ok;
		_ -> {error, is_up, Ip}
	end.

is_stale(Ip) ->
	Ut = tests(ip(), un(), pw()),
	Ct = tests(Ip, cun(), cpw()),
	lists:sort(Ut) /= lists:sort(Ct).

% basics
req_get(Url) ->
	ibrowse:send_req(Url, [], get).

db(Ip, Un, Pw, DbName) ->
	Host = Ip,
	Port = 5984,
	Prefix = "",
	AdminUser = Un,
	AdminPassword = Pw,
	Options = [{basic_auth, {AdminUser, AdminPassword}}],
	Cn = couchbeam:server_connection(Host, Port, Prefix, Options),
	{ok, DB} = couchbeam:open_db(Cn, DbName, []),
	DB.

b2l(null) -> "";
b2l(X) -> binary_to_list(X).

% config
ip() -> "121.241.73.173".
un() -> "wst".
pw() -> "MH12engr1UP12".
cun() -> "wst".
cpw() -> "MH12engr1".
dbs() -> ["an", "config", "img", "open", "qb", "rp", "tm", "wst", "_users"].
data_dbs() -> ["an", "img", "qb", "_users", "tm"].

sec("wst") -> "{\"admins\": {\"roles\" : [\"_admin\"]}}";
sec("config") -> "{\"admins\": {\"roles\" : [\"_admin\"]}}";
sec("open") -> "{\"admins\": {\"roles\" : [\"_admin\"]}}";
sec("qb") -> "{\"admins\": {\"roles\" : [\"_admin\"]}, \"readers\": {\"roles\" : [\"student\"]}}";
sec("img") -> "{\"admins\": {\"roles\" : [\"_admin\"]}, \"readers\": {\"roles\" : [\"student\"]}}";
sec("an") -> "{\"admins\": {\"roles\" : [\"_admin\"]}, \"readers\": {\"roles\" : [\"admin\"]}}";
sec("tm") -> "{\"admins\": {\"roles\" : [\"_admin\"]}, \"readers\": {\"roles\" : [\"admin\"]}}";
sec("rp") -> "{\"admins\": {\"roles\" : [\"_admin\"]}, \"readers\": {\"roles\" : [\"admin\"]}}";
sec("_users") -> "{\"admins\": {\"roles\" : [\"_admin\"]}, \"readers\": {\"roles\" : [\"student\", \"admin\"]}}";
sec(_) -> "_".

rep(ToIp, Db) -> "{\"source\": \"" ++ "setup-" ++ Db ++ "\", \"target\":\"" ++ root_college(ToIp) ++ "/" ++ Db ++ "\"}".

rep_data(Cd, ToIp, "_users" = Db) -> "{\"source\": \"" ++ Db ++ "\", \"target\":\"" ++ root_college(ToIp) ++ "/" ++ Db ++ "\", \"filter\": \"um_d/f_college\", \"query_params\": {\"college_code\": \"" ++ Cd ++ "\"}}";
rep_data(_, ToIp, Db) -> "{\"source\": \"" ++ Db ++ "\", \"target\":\"" ++ root_college(ToIp) ++ "/" ++ Db ++ "\"}".

root_uop() ->
	"http://wst:MH12engr1UP12@121.241.73.173:5984".

root_college(Ip) ->
	"http://wst:MH12engr1@" ++ Ip ++ ":5984".

% tests
t_admins() ->
	[["org.couchdb.user:02admin","02", "Maharashtra Institute of Technology, Pune","Tushar Gandre", "9545451123","115.111.61.71","5984"], ["org.couchdb.user:05admin","05", "Pune Institute Of Computer Technology","Pradeep Parkhi", "9822111009","124.247.239.77","5984"]].

% ui
flash(Term) ->
	helper_ui:flash(lists:flatten(io_lib:format("~p", [Term]))).

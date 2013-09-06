-module(oe2tests).
-compile(export_all).
-include_lib("records.hrl").

getdb() ->
	"oetests".

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

active() ->
	Docs = db:getdocs(getdb()),
	lists:foldl(fun(D, Acc) ->
		Fs = helper_api:doc2fields({ok, D}),
		Id = fields:find(Fs, '_id'),
		Name = fields:find(Fs, testname),
		Acc ++ [{
			Id#field.uivalue,
			Name#field.uivalue
		}]
	end, [], Docs).

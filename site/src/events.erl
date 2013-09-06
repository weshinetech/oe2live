-module(events).
-compile(export_all).
-include_lib("records.hrl").

get(create) ->
	#jevent {
		id=create,
		mode=?CREATE,
		label=locale:get(create),
		ignore=[]
	};

get(edit) ->
	#jevent {
		id=edit,
		mode=?EDIT,
		label=locale:get(edit),
		ignore=[]
	};

get(login) ->
	#jevent {
		id=login,
		mode=?CREATE,
		label=locale:get(login),
		ignore=[]
	};

get(export) ->
	#jevent {
		id=export,
		bind=false,
		mode=?EXPORT,
		label=locale:get(export),
		ignore=[]
	};

get(show_create) ->
	#jevent {
		id=show_create,
		mode=?CREATE,
		label=locale:get(show_create),
		ignore=[]
	};

get(export_create) ->
	#jevent {
		id=export_create,
		mode=?CREATE,
		label=locale:get(export_create),
		ignore=[]
	};

get(Event) ->
	helper:print(Event),
	throw(event_does_not_exist).

-module(locale).
-compile(export_all).

get(Id) ->
	get(english, Id).

get(english, Id) ->
	en:get(Id).

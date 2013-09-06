-module(utils_oe_manage).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(utils_oe_manage).

heading() ->
	locale:get(utils_oe_manage).

layout() ->
	case utils_oe:admin(wf:q(code)) of
		undefined -> "<pre>?code=02</pre>";
		Values -> [
			layout:g6(layout:frame(layout_details(Values))),
			layout:g3(layout:frame(layout_events(Values))),
			layout:g3(layout:frame(layout_links(Values))),
			layout:g0()
			]
	end.

layout_details(Values) ->
	lists:foldl(fun(V, Acc) ->
		Acc ++ [
			layout:g12(V),
			layout:g12(#br {}),
			layout:g0()
		]
	end, [], Values).

layout_links([_, _, _, _, _, Ip]) -> [
		#link {url="http://" ++ Ip ++ ":5984" ++ "/_utils", text="utils"},
		#hr {},
		#link {url="http://" ++ Ip ++ ":5984" ++ "/wst/_design/wst/index.html", text="app"}
].

layout_events([Cd, _, _, _, _, Ip]) -> [
	#button {text="is_up?", postback={is_up, Ip}},
	#hr {},
	#button {text="is_stale?", postback={is_stale, Ip}},
	#hr {},
	#button {text="app", postback={setup_app, Ip}},
	#hr {},
	#button {text="data", postback={setup_data, Ip, Cd}},
	#hr {},
	#button {text="nuke", postback={nuke, Ip}}
].

event({is_up, Ip} = E) ->
	utils_oe:flash({E, utils_oe:is_up(Ip)});

event({is_stale, Ip} = E) ->
	utils_oe:flash({E, utils_oe:is_stale(Ip)});

event({setup_app, Ip} = E) ->
	utils_oe:flash({E, utils_oe:setup_app(Ip)});

event({setup_data, Ip, Cd} = E) ->
	utils_oe:flash({E, utils_oe:setup_data(Cd, Ip)});

event({nuke, Ip} = E) ->
	case utils_oe:is_stale(Ip) of
		true -> utils_oe:flash({E, utils_oe:nuke(Ip)});
		false -> utils_oe:flash({E, "data is not stale. will not nuke"})
	end;

event(_) ->
	utils_oe:flash("event not handled").

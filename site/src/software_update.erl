-module (software_update).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(software_update_title).

heading() ->
	locale:get(software_update_heading).

layout() ->
	helper_admin:layout(?MODULE).

layout(software_update) ->
	Elements = [
		layout:g(10, #panel {class="mylabel", body=locale:get(msg_software_update_alert)}),
		layout:g(10, #panel {class="mylabel", id=myversion, body=locale:get(msg_software_update_myversion) ++ getMyversion()}),
		layout:g(10, #panel {class="mylabel", id=repoversion, body=locale:get(msg_software_update_repoversion) ++ locale:get(msg_checking)}),
		layout:g(10, #panel {class="mylabel", id=updatearea, body=[]})
	],
	wf:wire(#event{type=timer, delay=1000, postback=repoversion}),
	Elements.

event(update) ->
	os:cmd("git pull"),
	wf:update(myversion, locale:get(msg_software_update_myversion) ++ getMyversion()),
	wf:update(updatearea, []);

event(repoversion) ->
	checkRepoversion();

event(Event) ->
	helper:print(Event).

getMyversion() ->
	case file:read_file("etc/oe2version") of
		{ok, Version} -> helper:b2l(Version);
		_ -> locale:get(msg_unknown)
	end.

checkRepoversion() ->
	case helper:httpget(configs:get(url_repo_version)) of
		{error, _} ->
			wf:update(repoversion, locale:get(msg_software_update_repoversion) ++ locale:get(msg_unknown));
		Body ->
			[Rv | _] = string:tokens(Body, "\n"),
			[Mv | _] = string:tokens(getMyversion(), "\n"),
			case Rv == Mv of
				true -> ok;
				false -> show_update_button()
			end,
			wf:update(repoversion, locale:get(msg_software_update_repoversion) ++ Rv)
	end.

show_update_button() ->
	wf:update(updatearea, #button {class="btn btn-danger", postback=update, text=locale:get(msg_update_software)}).
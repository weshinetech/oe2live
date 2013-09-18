-module(unauthorised).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(unauthorised_title).

heading() ->
	locale:get(unauthorised_heading).

layout() -> [
	#panel {class="oe2form", body=locale:get(msg_unquthorised)},
	#panel {class="mycenter", body=[
		#hr {class="myseparator"},
		#link {class="mylabel label label-default", url="/login", text=locale:get(candidate_login)},
		#link {class="mylabel label label-default", url="/instructions", text=locale:get(instructions_click)},
		#link {class="mylabel label label-default", url="/profile_login", text=locale:get(profile_login)}
	]}
].

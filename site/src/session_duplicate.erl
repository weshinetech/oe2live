-module(session_duplicate).
-compile(export_all).
-include_lib("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(session_duplicate_title).

heading() ->
	locale:get(session_duplicate_heading).

layout() -> [
	#panel {class="mycenter", body=[
		#span {class="myheading", text=locale:get(session_duplicate_heading)}
	]},
	#panel {class="mycenter", body=[
		#panel {class="oe2form", body=[
			#span {text=locale:get(msg_session_duplicate_desc)}
		]}
	]},
	#panel {class="mycenter", body=[
		#hr {class="myseparator"},
		#link {class="mylabel label label-default", url="/login", text=locale:get(candidate_login)},
		#link {class="mylabel label label-default", url="/instructions", text=locale:get(instructions_click)},
		#link {class="mylabel label label-default", url="/profile_login", text=locale:get(profile_login)}
	]}
].
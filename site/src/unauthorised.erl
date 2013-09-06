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

layout() ->
	#panel {class="oe2form", body=locale:get(msg_unquthorised)}.

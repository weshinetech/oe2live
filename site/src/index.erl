-module (index).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(index_welcome).

heading() ->
	locale:get(index_heading).

layout() ->
	helper_admin:layout(?MODULE).

layout(index) ->
	#panel {body=[
		#h4 {text=configs:get(customer_text)},
		#hr {},
		#span {text=locale:get(msg_index_welcome)}
	]}.
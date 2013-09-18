-module(instructions).
-compile(export_all).
-include_lib("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(instructions_title).

heading() ->
	locale:get(instructions_heading).

layout() -> [
	#panel {class="row-fluid mycenter", body=[
		#span {class="myheading", text=locale:get(instructions)}
	]},
	#br{},
	#panel {class="row-fluid", body=[
		#table {class="table table-hover", rows=[
			#tablerow {cells=[
				#tableheader {body=#span {text=locale:get(instructions_sno)}},
				#tableheader {body=#span {text=locale:get(instructions_section)}},
				#tableheader {body=#span {text=locale:get(instructions_description)}}
			]},
			lists:map(fun({I, Section, Text}) ->
				#tablerow {cells=[
					#tablecell {body=#span {text=I}},
					#tablecell {body=#span {text=Section}},
					#tablecell {body=#span {text=Text}}
				]}
			end, instructions())
		]}
	]},
	#panel {class="row-fluid mycenter", body=[
		#hr {class="myseparator"},
		#link {class="mylabel label label-default", url="/login", text=locale:get(candidate_login)},
		#link {class="mylabel label label-default", url="/profile_login", text=locale:get(profile_login)}
	]}
].

instructions() -> [
	{1, locale:get(instructions_section_session), locale:get(instructions_attempts)},
	{2, locale:get(instructions_section_session), locale:get(instructions_refresh)},
	{3, locale:get(instructions_section_session), locale:get(instructions_submit)},
	{4, locale:get(instructions_section_timer), locale:get(instructions_timer)},
	{5, locale:get(instructions_section_browser), locale:get(instructions_browsers)}
].
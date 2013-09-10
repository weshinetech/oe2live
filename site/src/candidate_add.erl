-module (candidate_add).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(candidate_add_title).

heading() ->
	locale:get(candidate_add_heading).

layout() ->
	helper_admin:layout(?MODULE).

layout(candidate_add) ->
	{Fs, Es} = layout:get(?CREATE, helper_ui:fields(?MODULE), helper_ui:events(eids())),
	layout:g(10, layout:form(oe2form_horizontal, ?MODULE, {Fs, Es})).

fids() -> [
	testsactive,
	oeuserseatnumber,
	oeuserfullname,
	oeuseraddname,
	oeuseraddreason
].

eids() -> [
	create
].

event({timer_flash, _} = E) ->
	helper_ui:event(E);

event(create) ->
	Fields = fields:uivalue(helper_ui:fields(?MODULE)),
	onresult(oeusers:candidate_addition(Fields));

event(Event) ->
	helper:print(Event).

onresult({ok, _}) ->
	helper_ui:flash(success, locale:get(msg_candidate_add_success));
onresult({error, exists}) ->
	helper_ui:flash(error, locale:get(msg_candidate_add_error_exists));
onresult({error, _}) ->
	helper_ui:flash(error, locale:get(msg_candidate_add_error)).
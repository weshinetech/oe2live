-module(oe2test).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(oe2test_title).

heading() ->
	locale:get(oe2test_heading).

layout() ->
	{Fs, Es} = layout:get(wf:q(mode),
		helper_ui:fields(?MODULE, oe2tests:get(wf:q(id()))),
		helper_ui:events(eids())),
	layout:form(oe2form_horizontal, ?MODULE, {Fs, Es}).

id() ->
	oe2testid.

fids() -> [
	oe2testid,
	'_rev',
	oe2testname,
	oe2testdescription,
	oe2teststatus
].

eids() -> [
	create, edit
].

event(create) ->
	Fields = fields:uivalue(helper_ui:fields(?MODULE)),
	helper_ui:flash(oe2tests:create(Fields));

event(edit) ->
	Fields = fields:uivalue(helper_ui:fields(?MODULE)),
	helper_ui:flash(oe2tests:update(Fields));

event(_) ->
	throw(event_not_handled).

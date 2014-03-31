-module(login).
-compile(export_all).
-include_lib("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:clear_session(),
	wf:clear_session(),
	myauth:main(?MODULE).

title() ->
	locale:get(login_title).

heading() ->
	locale:get(login_heading).

layout() ->
	{Fs, Es} = layout:get(?CREATE, fields(), helper_ui:events(eids())),
	[
		layout:form(oe2form, ?MODULE, {Fs, Es}),
		#hr {class="myseparator"},
		#panel {class="mycenter", body=[
			#link {class="mylabel label label-default", url="/instructions", text=locale:get(instructions_click)},
			#link {class="mylabel label label-default", url="/profile_login", text=locale:get(profile_login)}
		]}
	].

fids() -> [
	username,
	password_bcrypt,
	testsactive
].

eids() -> [
	login
].

fields() ->
	[FU, FP, FT] = helper_ui:fields(?MODULE),
	[FU#field{validators=[required]}, FP, FT].

event(_) ->
	[FUser, FPass, FTest] = fields:uivalue(helper_ui:fields(?MODULE)),
	validateUser(oeusers:getuser(FTest#field.uivalue, FUser#field.uivalue), FPass#field.uivalue, FTest#field.uivalue).

onloginfailed() ->
	helper_ui:flash(error, locale:get(login_failed)).
onloginfailed(Type) ->
	helper_ui:flash(error, locale:get(Type)).

validateUser({error, _}, _, _) ->
	onloginfailed();
validateUser([], _, _) ->
	onloginfailed();
validateUser(Fs, Password, TestId) ->
	validatePassword(Fs, fields:find(Fs, oeusertoken), Password, TestId).

validatePassword(Fs, #field {uivalue=Password}, Password, TestId) ->
	validateTestState(Fs, fields:find(Fs, oeuserexamstate), TestId);
validatePassword(_, _, _, _) ->
	onloginfailed(login_failed_password).

validateTestState(_, undefined, _) ->
	onloginfailed();
validateTestState(_, #field {uivalue=?ACTIVE}, _) ->
	onloginfailed(login_failed_active_contact_admin);
validateTestState(_, #field {uivalue=?COMPLETED}, _) ->
	onloginfailed(login_failed_expired);
validateTestState(Fs, _, TestId) ->
	TFs = oe2tests:get(TestId),
	LoginTimes = helper:s2i(fields:finduival(Fs, oeuserlogintimes)),
	MaxLogins = helper:s2i(fields:finduival(TFs, testmaxlogins)),
	validateLoginTimes(Fs, TFs, LoginTimes, MaxLogins).

validateLoginTimes(_, _, LoginTimes, MaxLogins) when LoginTimes >= MaxLogins ->
	onloginfailed(login_failed_maxlogins);
validateLoginTimes(Fs, TFs, _, _) ->
	UId = fields:getuivalue(Fs, '_id'),
	myauth:username(UId),
	myauth:userfields(Fs),
	myauth:testfields(TFs),
	myauth:role("candidate"),
	cache:session_id(myauth:username(), wf:session_id()),
	helper:redirect("/exam").
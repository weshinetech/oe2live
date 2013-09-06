-module(profile_login).
-compile(export_all).
-include_lib("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	wf:clear_session(),
	myauth:clear_session(),
	myauth:main(?MODULE).

title() ->
	locale:get(profile_login_title).

heading() ->
	locale:get(profile_login_heading).

layout() ->
	{Fs, Es} = layout:get(?CREATE, fields(), helper_ui:events(eids())),
	[
		#h2 {class="mycenter", text=configs:get(customer_text)},
		layout:form(oe2form, ?MODULE, {Fs, Es}),
		#hr {class="myseparator"},
		#panel {class="mycenter", body=[
			#link {class="mylabel label label-default", url="/instructions", text=locale:get(instructions_click)},
			#link {class="mylabel label label-default", url="/login", text=locale:get(login)}
		]}
	].

fids() -> [
	username,
	password_bcrypt
].

eids() -> [
	login
].

fields() ->
	[FU, FP] = helper_ui:fields(?MODULE),
	[FU#field{validators=[required]}, FP].

event({timer_flash, _} = E) ->
	helper_ui:event(E);
event(_) ->
	[FUser, FPass] = fields:uivalue(helper_ui:fields(?MODULE)),
	validateUser(profiles:getuser(FUser#field.uivalue), FPass#field.uivalue).

validateUser({error, _}, _) ->
	onloginfailed();
validateUser([], _) ->
	onloginfailed();
validateUser(Fs, Password) ->
	validatePassword(Fs, fields:find(Fs, password_bcrypt), Password).

validatePassword(Fs, #field {uivalue=PasswordBcrypt}, Password) ->
	case {ok, PasswordBcrypt} =:= bcrypt:hashpw(Password, PasswordBcrypt) of
		true ->
			FUser = fields:find(Fs, '_id'),
			Role = fields:find(Fs, profiletype),
			myauth:username(FUser#field.uivalue),
			myauth:role(Role#field.uivalue),
			myauth:userfields(Fs),
			helper:redirect("/index");
		false ->
			onloginfailed()
	end.

onloginfailed() ->
	helper_ui:flash(error, locale:get(login_failed)).
onloginfailed(Type) ->
	helper_ui:flash(error, locale:get(Type)).
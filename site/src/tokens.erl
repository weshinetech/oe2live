-module (tokens).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(tokens_title).

heading() ->
	locale:get(tokens_heading).

layout() ->
	helper_admin:layout(?MODULE).

layout(tokens) ->
	{Fs, Es} = layout:get(?CREATE, helper_ui:fields(?MODULE), helper_ui:events(eids())),
	[
		layout:g(10, layout:form(oe2form_simple, ?MODULE, {Fs, Es})),
		layout:g(10, #panel {id=download, body=[]}),
		layout:g(10, #panel {id=result, body=[]})
	].

fids() -> [
	testsactive
].

eids() -> [
	show_create,
	export_create
].

event({download, FTest}) ->
	Url = "/download?type=tokens&testid=" ++ FTest#field.uivalue,
	helper:redirect(Url);

event(show_create) ->
	[FTest] = fields:uivalue(helper_ui:fields(?MODULE)),
	R = oeusers:getusers(FTest#field.uivalue),
	wf:update(result, layout:table(R, [oeuserseatnumber, oeuserfullname, oeusertoken], 1)),
	show_download(FTest);

event(Event) ->
	helper:print(Event).

show_download(FTest) ->
	wf:update(download, [
		#button {class="btn", postback={download, FTest}, text=locale:get(msg_download)},
		#span {text=" - "},
		#link {class="btn", url="/print?type=tokens&testid=" ++ FTest#field.uivalue, text=locale:get(msg_print)}
	]).
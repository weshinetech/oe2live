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
	{Fs, Es} = layout:get(?CREATE, helper_ui:fields(?MODULE), helper_ui:events(eids())),
	helper_ui:fullpage(layout:g(10, layout:form(oe2form_horizontal, ?MODULE, {Fs, Es}))).

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

event(create) ->
	Fields = fields:uivalue(helper_ui:fields(?MODULE)),
	NewFields = lists:foldl(fun(F, Acc) ->
		case F#field.id of
			'_id' ->
				Acc ++[F];
			'_rev' ->
				Acc ++[F];
			testsactive ->
				Acc ++[F];
			_ ->
				NewVal = string:to_upper(F#field.uivalue),
				Acc ++ [F#field {uivalue=NewVal}]
		end
	end, [], Fields),
	onresult(oeusers:candidate_addition(NewFields));

event(Event) ->
	helper:print(Event).

onresult({ok, _} =Res) ->
	U = helper_api:doc2fields(Res),
	TId = wf:q(testsactive),
	UId = fields:getuivalue(U, '_id'),
	Url = "/candidate?oeuserseatnumber=" ++ UId ++ "&oetestid=" ++ TId,
	helper:redirect(Url);
onresult({error, exists}) ->
	helper_ui:flash(error, locale:get(msg_candidate_add_error_exists));
onresult({error, _}) ->
	helper_ui:flash(error, locale:get(msg_candidate_add_error)).
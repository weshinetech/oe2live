-module(videoresponse).
-compile(export_all).
-include_lib("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

%---------------------------------------------------------------------------------------------------
% AUTH
%---------------------------------------------------------------------------------------------------
main() ->
	myauth:main(?MODULE).

title() ->
	locale:get(videoresponse_title).

heading() ->
	locale:get(videoresponse_heading).

%---------------------------------------------------------------------------------------------------
% LAYOUT
%---------------------------------------------------------------------------------------------------
layout() ->
	Fs = myauth:userfields(),
	case wf:q(completed) of
		"true" ->
			handle_state(Fs, "oe_videoresponse_response_state_completed");
		_ ->
			handle_state(Fs, fields:getuivalue(Fs, oe_videoresponse_response_state))
	end.

%---------------------------------------------------------------------------------------------------
% HANDLE VIDEORESPOSE STATES
%---------------------------------------------------------------------------------------------------
handle_state(Fs, "oe_videoresponse_response_state_new") ->
	update_state(Fs, "oe_videoresponse_response_state_partial"),
	get_iframe(Fs);

handle_state(Fs, "oe_videoresponse_response_state_partial") ->
	get_iframe(Fs);

handle_state(Fs, "oe_videoresponse_response_state_completed" = S) ->
	update_state(Fs, S),
	helper:redirect("/exam");

handle_state(_, _) ->
	helper:redirect("/login").

%---------------------------------------------------------------------------------------------------
% USER INFO DISPLAY
%---------------------------------------------------------------------------------------------------
nav_left() ->
	Fs = myauth:userfields(),
	#panel { body=[
		#panel {body=fields:getuivalue(Fs, oeuserfullname)},
		#panel {body="[" ++ fields:getuivalue(Fs, oeusercentercode) ++ ", " ++ fields:getuivalue(Fs, '_id') ++ "]"}
	]}.
nav_right() ->
	Fs = myauth:testfields(),
	#panel { body=[
		#panel {body=fields:getuivalue(Fs, testname)}
	]}.

%---------------------------------------------------------------------------------------------------
% HELPERS
%---------------------------------------------------------------------------------------------------
get_videoresponse_url(ResponseId) ->
	OnComplete = wf:url_encode(lists:flatten(io_lib:format("http://~s/videoresponse?completed=true", [myauth:host()]))),
	lists:flatten(io_lib:format("https://videoresponse.in/response/en/#response/~s?oncomplete=~s", [ResponseId, OnComplete])).

get_iframe(Fs) ->
	Url = get_videoresponse_url(fields:finduival(Fs, oe_videoresponse_responseid)),
	Iframe = io_lib:format("<iframe id='videoresponse_iframe' width='100%' height='800' frameborder='0' scrolling='no' src='~s'></iframe>", [Url]),
	Iframe.

update_state(Fs, NewState) ->
	S = fields:find(Fs, oe_videoresponse_response_state),
	NewFs = fields:delete(Fs, oe_videoresponse_response_state) ++ [S#field {uivalue=NewState}],
	TestId = fields:getuivalue(myauth:testfields(), '_id'),
	Res = oeusers:update(?DB_USERS ++ TestId, NewFs),
	myauth:userfields(helper_api:doc2fields(Res)).

%---------------------------------------------------------------------------------------------------
% END
%---------------------------------------------------------------------------------------------------
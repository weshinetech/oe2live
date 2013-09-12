-module(myauth).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").

% session info
username(Username) ->
	helper:session(username, Username).

username() ->
	helper:session(username).

userfields(Fs) ->
	helper:session(userfields, Fs).

userfields() ->
	helper:session(userfields).

testfields(Fs) ->
	helper:session(testfields, Fs).

testfields() ->
	helper:session(testfields).

role(Role) ->
	helper:session(role, Role).

role() ->
	helper:session(role).

pageloaded(Page, true) ->
	helper:session({pageloaded, Page}, true).

pageloaded(Page) ->
	helper:session({pageloaded, Page}) == true.

clear_session() ->
	username(undefined),
	role(undefined).

is_authenticated() ->
	is_valid_session().

is_valid_session() ->
	username() /= undefined.

is_valid_access(Module) ->
	helper_access:get(Module).

oecentercode() ->
	fields:finduival(userfields(), oecentercode).

% request ip
ip() ->
	case wf:peer_ip() of
		{A, B, C, D} ->
			string:join([helper:i2s(A), helper:i2s(B),
				helper:i2s(C), helper:i2s(D)], ".");
		_ ->
			[]
	end.

% auth entry point
main(Module) ->
	handle_session(is_valid_session(), Module).

% verify session
handle_session(_, instructions) ->
	load_template(instructions);
handle_session(_, session_duplicate) ->
	load_template(session_duplicate);
handle_session(true, Module) ->
	handle_access(is_valid_access(Module), Module);
handle_session(false, login) ->
	load_template(login);
handle_session(false, profile_login) ->
	load_template(profile_login);
handle_session(false, _) ->
	helper:redirect("/login").

% verify access to module
handle_access(true, Module) ->
	load_template(Module);
handle_access(false, _) ->
	helper:redirect("/unauthorised").

load_template(login) ->
	#template {file="./site/templates/login.html"};
load_template(exam) ->
	#template {file="./site/templates/exam.html"};
load_template(_) ->
	#template {file="./site/templates/bare.html"}.

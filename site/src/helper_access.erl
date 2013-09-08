-module(helper_access).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

get(login) ->
	true;
get(profile_login) ->
	true;
get(instructions) ->
	true;
get(unauthorised) ->
	true;
get(M) ->
	Mode = case wf:q(mode) of
		undefined -> ?CREATE;
		[] -> ?CREATE;
		Else -> Else
	end,
	allowed(M, helper:l2a(Mode)).

allowed(M, Mode) ->
	Allowed = access(M, Mode),
	Role = helper:l2a(helper:session(role)),
	lists:member(Role, Allowed).

access(test, search) -> [admin];
access(test, view) -> [admin, oeadmin, oestaff];
access(test, edit) -> [admin, oeadmin];
access(test, create) -> [admin, oeadmin];

access(exam, _) -> [candidate];

access(index, _) -> [admin, oeadmin, oestaff];
access(tokens, _) -> [admin, oeadmin, oestaff];
access(candidate_status, _) -> [admin, oeadmin, oestaff];
access(candidate_search, _) -> [admin, oeadmin, oestaff];
access(report_generate, _) -> [admin, oeadmin, oestaff];

access(download, _) -> [admin, oeadmin, oestaff];

access(_, _) -> [].

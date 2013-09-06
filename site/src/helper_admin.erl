-module(helper_admin).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

layout(Mod) -> [
	layout:g(2, links()),
	layout:g(10, Mod:layout(Mod))
].

links() ->
	#panel {class="mysidebar", body=[
		#br{},
		#span {class="mylabel", text="LINKS"},
		#br{},
		#link {class="mylabel", url="/index", text=locale:get(admin_link_home)},
		#br{},
		#link {class="mylabel", url="/tokens", text=locale:get(admin_link_tokens)},
		#br{},
		% #link {class="mylabel", url="/candidate_search", text=locale:get(admin_link_candidate_search)},
		% #br{},
		#link {class="mylabel", url="/candidate_status", text=locale:get(admin_link_candidate_status)},
		#br{},
		#link {class="mylabel", url="/report_generate", text=locale:get(admin_link_report_generate)},
		#br{},
		#link {class="mylabel", url="/profile_login", text=locale:get(admin_link_logout)}
	]}.

layout_oeuser_rows(Users, Ids, Type) ->
	lists:map(fun(U) ->
		#tablerow {class=Type, cells=layout:row_cells(U, Ids)}
	end, Users).


layout_oeuser_header(Ids) ->
	layout:table_header(Ids, 1).
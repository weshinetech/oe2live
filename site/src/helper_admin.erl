-module(helper_admin).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include_lib("records.hrl").

links() ->
	#panel {body=[
		#br{},
		#span {class="mylabel", text="LINKS"},
		#br{},
		#link {class="mylabel", url="/index", text=locale:get(admin_link_home)},
		#br{},
		#link {class="mylabel", url="/candidate_add", text=locale:get(admin_link_candidate_add)},
		#br{},
		#link {class="mylabel", url="/profile_login", text=locale:get(admin_link_logout)}
	]}.

layout_oeuser_rows(Users, Ids, Type) ->
	lists:map(fun(U) ->
		#tablerow {class=Type, cells=layout:row_cells(U, Ids)}
	end, Users).


layout_oeuser_header(Ids) ->
	layout:table_header(Ids, 1).

menu() ->
	menu_1(myauth:role()).

menu_1(Role) when Role == "oestaff"; Role == "admin" ->
	Html = "<div class='nav pull-right admin-menu hidden-print'>
				<li class='dropdown'>
					<a data-toggle='dropdown' href='#' class='dropdown-toggle'>
						<span class='mylabel label label-default'>~s</span><i class='pull-right icon-chevron-down'></i>
					</a>
					<ul role='menu' class='dropdown-menu clear-fix'>
						<li><a href='/index'>~s</a></li>
						<li><a href='/candidate_add'>~s</a></li>
						<li><a href='/profile_login'>~s</a></li>
					</ul>
				</li>
			</div>",
	lists:flatten(io_lib:format(Html, [
		myauth:username(),
		locale:get(admin_link_home),
		locale:get(admin_link_candidate_add),
		locale:get(admin_link_logout)
	]));

menu_1(_) ->
	[].
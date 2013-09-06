-module(renderer).
-compile(export_all).
-include_lib("nitrogen_core/include/wf.hrl").
-include("records.hrl").

% wire
wire(_, _, undefined, _) -> ok;
wire(false, _, _, _) -> ok;
wire(true, Id, Action, Validators) ->
	List = lists:foldl(fun(V, Acc) ->
		Acc ++ [
			#custom {text=validators:tip(V), function=validators:get(V)}
		]
	end, [], Validators),
	wf:wire(Action, Id, #validate {validators=List}).

% textbox
get(textbox) ->
	fun(Md, Ac, #field {id=I, uivalue=V, postback=P, label=L, validators=Va}) ->
		{Validate, FE} = if 
			Md == ?VIEW ->
				{false, #span {text=V}};
			true ->
				{true, #textbox {id=I, text=V, placeholder=L, postback=P}}
		end,
		wire(Validate, I, Ac#jevent.id, Va),
		{L, FE}
	end;

% password_bcrypt
get(password_bcrypt) ->
	fun(Md, Ac, #field {id=I, uivalue=V, postback=P, label=L, validators=Va}) ->
		{Validate, FE} = if 
			Md == ?VIEW ->
				{false, #span {text=locale:get(password_hidden)}};
			Md == ?EDIT ->
				helper:state(I, V),
				{false, [#hidden {id=I}, #span {text=locale:get(password_hidden)}]};
			P /= undefined ->
				{true, #password {id=I, placeholder=L, postback=P}};
			true ->
				{true, #password {id=I, placeholder=L}}
		end,
		wire(Validate, I, Ac#jevent.id, Va),
		{L, FE}
	end;

% hidden
get(hidden) ->
	fun(Md, _, #field {id=I, type=T, uivalue=V}) ->
		{_, FE} = if 
			Md == ?VIEW ->
				{false, []};
			T == primarykey ->
				{false, []};
			T == foreignkey, V /= undefined ->
				{false, [#hidden {id=I, text=V}]};
			V /= undefined ->
				{false, #hidden {id=I, text=V}};
			true ->
				{false, []}
		end,
		{[], FE}
	end;

% textarea
get(textarea) ->
	fun(Md, Ac, #field {id=I, uivalue=V, label=L, validators=Va}) ->
		{Validate, FE} = if 
			Md == ?VIEW ->
				{false, #span {text=V}};
			V /= undefined ->
				{true, #textarea {id=I, text=V, placeholder=L}};
			true ->
				{true, #textarea {id=I, placeholder=L}}
		end,
		wire(Validate, I, Ac#jevent.id, Va),
		{L, FE}
	end;

% html
get(html) ->
	fun(Md, Ac, #field {id=I, uivalue=V, label=L, validators=Va}) ->
		{Validate, FE} = if 
			Md == ?VIEW ->
				{false, V};
			V /= undefined ->
				{true, #textarea {id=I, text=V}};
			true ->
				{true, #textarea {id=I}}
		end,
		wire(Validate, I, Ac#jevent.id, Va),
		{L, FE}
	end;

% dropdown
get(dropdown) ->
	fun(Md, Ac, #field {id=I, uivalue=V, postback=P, label=L, validators=Va} = F) ->
		{Validate, FE} = if 
			Md == ?VIEW ->
				{false, #span {text=locale:get(helper:l2a(V))}};
			true ->
				{true, #dropdown {id=I, postback=P, options=lists:foldl(fun({OId, OText}, Acc) ->
							case OId of
								V -> Acc ++ [#option {text=OText, value=OId, selected=true}];
								_ -> Acc ++ [#option {text=OText, value=OId}]
							end
						end, [], helper_options:get(helper:id(F)))
				}}
		end,
		wire(Validate, I, Ac#jevent.id, Va),
		{L, FE}
	end;
	
% fixed
get(fixed) ->
	fun(Md, _, #field {id=I, uivalue=V}) ->
		{_, FE} = if 
			Md == ?VIEW ->
				{false, []};
			V /= undefined ->
				{false, #hidden {id=I, text=V}};
			true ->
				{false, []}
		end,
		{[], FE}
	end;

% date
get(date) ->
	fun(Md, Ac, #field {id=I, uivalue=V, label=L, validators=Va}) ->
		{Validate, FE} = if 
			Md == ?VIEW ->
				{false, #span {text=V}};
			V /= undefined ->
				{true, #datepicker_textbox {id=I, text=V}};
			true ->
				{true, #datepicker_textbox {id=I}}
		end,
		wire(Validate, I, Ac#jevent.id, Va),
		{L, FE}
	end;

% time
get(time) ->
	fun(Md, Ac, #field {id=I, uivalue=V, label=L, validators=Va}) ->
		{Validate, FE} = if 
			I == createdon_time ->
				TNow = helper:time_t2s(helper:time_now()),
				{false, [#span {text=TNow}, #hidden {id=I, text=TNow}]};
			Md == ?VIEW ->
				{false, #span {text=V}};
			V /= undefined ->
				{true, #textbox {id=I, text=V}};
			true ->
				{true, #textbox {id=I, placeholder="hh:mm:ss"}}
		end,
		wire(Validate, I, Ac#jevent.id, Va),
		{L, FE}
	end;

% createdby
get(createdby) ->
	fun(Md, Ac, #field {id=I, uivalue=V, label=L, validators=Va}) ->
		Name = myauth:username(),
		{Validate, FE} = if 
			Md == ?CREATE->
				{false, [#span {text=Name}, #hidden {id=I, text=Name}]};
			Md == ?EDIT ->
				{false, [#span {text=V}, #hidden {id=I, text=V}]};
			true ->
				{false, #span {text=V}}
		end,
		wire(Validate, I, Ac#jevent.id, Va),
		{L, FE}
	end;

% createdon
get(createdon) ->
	fun(Md, Ac, #field {id=I, uivalue=V, label=L, validators=Va}) ->
		Today = helper:date_d2s(helper:date_today()),
		{Validate, FE} = if 
			Md == ?CREATE->
				{false, [#span {text=Today}, #hidden {id=I, text=Today}]};
			Md == ?EDIT ->
				{false, [#span {text=V}, #hidden {id=I, text=V}]};
			true ->
				{false, #span {text=V}}
		end,
		wire(Validate, I, Ac#jevent.id, Va),
		{L, FE}
	end;

% subfields
get(subfields) ->
	fun(Md, Ac, #field {label=L, subfields=S}) ->
		{L, layout:getfields(Md, Ac, S)}
	end;
get(address) -> ?MODULE:get(subfields);
get(years_months_days) -> ?MODULE:get(subfields);

get(T) ->
	helper:print(T),
	throw(renderer_not_available).

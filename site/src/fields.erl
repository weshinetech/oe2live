-module(fields).
-compile(export_all).
-include_lib("records.hrl").

% keys
get(profileid = I) -> hidden(I);
get(oetestid = I) -> hidden(I);

% hidden
get('_id' = I) -> hidden(I);
get('_rev' = I) -> hidden(I);

% textbox
get(username = I) -> F = textbox(I), F#field {postback=textbox_enterkey, validators=[username]};
get(testname = I) -> textbox(I);
get(firstname = I) -> textbox(I);
get(lastname = I) -> textbox(I);
get(middlename = I) -> F= textbox(I), F#field {validators=[]};
get(address_line_1 = I) -> textbox(I);
get(address_line_2 = I) -> F = textbox(I), F#field {validators=[]};
get(address_city = I) -> textbox(I);
get(address_state = I) -> textbox(I);
get(address_pincode = I) -> F = textbox(I), F#field {validators=[pincode]};
get(mobile = I) -> F = textbox(I), F#field {validators=[mobile]};
get(landline = I) -> F = textbox(I), F#field {validators=[mobile]};
get(email = I) -> F = textbox(I), F#field {validators=[email]};
get(testduration = I) -> F = textbox(I), F#field {validators=[integer]};
get(oecentercode = I) -> textbox(I);

% textarea
get(testdescription = I) -> textarea(I);

% dropdown
get(teststatus = I) -> dropdown(I);
get(testsactive = I) -> dropdown(I);
get(blood_group = I) -> dropdown(I);
get(address_country = I) -> dropdown(I);
get(gender = I) -> dropdown(I);
get(qualification = I) -> dropdown(I);
get(year = I) -> dropdown(I);
get(academic_branch = I) -> dropdown(I);
get(academic_course_year = I) -> dropdown(I);
get(academic_semester = I) -> dropdown(I);

% fixed
get(profiletype = I) -> fixed(I);
get(profiletype_admin) -> fixed(profiletype, admin);
get(profiletype_staff) -> fixed(profiletype, staff);
get(profiletype_student) -> fixed(profiletype, student);

% date
get(testdate = I) -> date(I);

% password
get(password_bcrypt = I) -> F = password_bcrypt(I), F#field{postback=textbox_enterkey};

% address
get(local_address = I) -> address(I);

% createdby
get(createdby = I) -> createdby(I);

% createdon
get(createdon = I) -> createdon(I);

% lists
get(questions =I) -> mylist:get(I);

% question
get(questionid = I) -> hidden(I);
get(questiondescription = I) -> html(I);
get(optiona = I) -> html(I);
get(optionb = I) -> html(I);
get(optionc = I) -> html(I);
get(optiond = I) -> html(I);
get(optione = I) -> html(I);
get(answer = I) -> textbox(I);
get(marks = I) -> textbox(I);
get(unit = I) -> textbox(I);

% user
get(oeuserid = I) -> hidden(I);
get(oeusercentercode = I) -> textbox(I);
get(oeuserseatnumber = I) -> textbox(I); 
get(oeuserfullname = I) -> textbox(I);
get(oeuseraddname = I) -> textbox(I);
get(oeuserlogintimes = I) -> textbox(I);
get(oeuserqna = I) -> mylist2:get(I);
get(oeusermarkers = I) -> mylist2:get(I);
get(oeuserreported = I) -> mylist2:get(I);
get(oeusertoken = I) -> textbox(I);
get(oeuserstarttime = I) -> textbox(I);
get(oeuserendtime = I) -> textbox(I);
get(oeusertimeleftseconds = I) -> textbox(I);
get(oeuserexamstate = I) -> dropdown(I);
get(oeuserscore = I) -> textbox(I);

% other
get(_) -> undefined.

% helper funcions to create fields
hidden(I) ->	?MODULE:get(hidden, I).
textbox(I) -> ?MODULE:get(textbox, I, locale:get(I)).
textarea(I) -> ?MODULE:get(textarea, I, locale:get(I)).
html(I) -> ?MODULE:get(html, I, locale:get(I)).
dropdown(I) -> ?MODULE:get(dropdown, I, locale:get(I)).
fixed(I) -> ?MODULE:get(fixed, I).
date(I) -> ?MODULE:get(date, I, locale:get(I)).
time(I) -> ?MODULE:get(time, I, locale:get(I)).
password_bcrypt(I) -> ?MODULE:get(password_bcrypt, I, locale:get(I)).
address(I) -> ?MODULE:get(address, I, locale:get(I)).
years_months_days(I) -> ?MODULE:get(years_months_days, I, locale:get(I)).
fixed(I, V) ->
	F = ?MODULE:get(fixed, I),
	F#field {uivalue=helper:a2l(V), dbvalue=helper:a2b(V)}.
createdby(I) -> ?MODULE:get(createdby, I, locale:get(I)).
createdon(I) -> ?MODULE:get(createdon, I, locale:get(I)).

% initialize field records
get(fixed, Id) ->
	#field {
		id=Id,
		type=fixed,
		renderer=renderer:get(fixed)
	};
get(hidden, Id) ->
	#field {
		id=Id,
		type=hidden,
		renderer=renderer:get(hidden)
	}.

get(password_bcrypt, Id, Label) ->
	#field {
		id=Id,
		type=password_bcrypt,
		label=Label,
		renderer=renderer:get(password_bcrypt),
		validators=[required, length4, noallspaces]
	};
get(textbox, Id, Label) ->
	#field {
		id=Id,
		type=textbox,
		label=Label,
		renderer=renderer:get(textbox),
		validators=[required]
	};
get(textarea, Id, Label) ->
	#field {
		id=Id,
		type=textarea,
		label=Label,
		renderer=renderer:get(textarea),
		validators=[required]
	};
get(html, Id, Label) ->
	#field {
		id=Id,
		type=html,
		label=Label,
		renderer=renderer:get(html),
		validators=[required]
	};
get(date, Id, Label) ->
	#field {
		id=Id,
		type=date,
		label=Label,
		renderer=renderer:get(date),
		validators=[required, date]
	};
get(time, Id, Label) ->
	#field {
		id=Id,
		type=time,
		label=Label,
		renderer=renderer:get(time),
		validators=[required, time]
	};
get(dropdown, Id, Label) ->
	#field {
		id=Id,
		type=dropdown,
		label=Label,
		renderer=renderer:get(dropdown),
		validators=[required]
	};
get(address, Id, Label) ->
	#field {
		id=Id,
		type=address,
		renderer=renderer:get(address),
		label=Label,
		subfields=?MODULE:get(subfields, Id, [address_line_1, address_line_2,
			address_city, address_state, address_pincode, address_country])
	};
get(years_months_days, Id, Label) ->
	#field {
		id=Id,
		type=years_months_days,
		renderer=renderer:get(years_months_days),
		label=Label,
		subfields=?MODULE:get(subfields, Id, [ymd_years, ymd_months, ymd_days])
	};
get(createdby, Id, Label) ->
	#field {
		id=Id,
		type=createdby,
		label=Label,
		renderer=renderer:get(createdby),
		validators=[]
	};
get(createdon, Id, Label) ->
	#field {
		id=Id,
		type=createdon,
		label=Label,
		renderer=renderer:get(createdon),
		validators=[]
	};

get(subfields, Id, FieldList) ->
	lists:foldl(fun(I, Acc) ->
		F = ?MODULE:get(I),
		Acc ++ [
			F#field {baseid=I, id=helper:id(Id, I)}
		]
	end, [], FieldList);

get(Type, Id, Label) ->
	helper:print(Type),
	helper:print(Id),
	helper:print(Label),
	throw(basic_field_does_not_exist).

uivalue(password_bcrypt, Id) ->
	case helper:state(Id) of
		undefined -> wf:q(Id);
		V -> V
	end;

uivalue(textbox, Id) ->
	wf:q(Id);

uivalue(hidden, Id) ->
	wf:q(Id);

uivalue(primarykey, Id) ->
	wf:q(Id);

uivalue(foreignkey, Id) ->
	wf:q(Id);

uivalue(textarea, Id) ->
	wf:q(Id);

uivalue(html, Id) ->
	wf:q(Id);

uivalue(dropdown, Id) ->
	wf:q(Id);

uivalue(date, Id) ->
	wf:q(Id);

uivalue(time, Id) ->
	wf:q(Id);

uivalue(fixed, Id) ->
	wf:q(Id);

uivalue(attachment, I) ->
	attachment:uivalue(I);

uivalue(createdby, I) ->
	wf:q(I);

uivalue(createdon, I) ->
	wf:q(I);

uivalue(mylist, I) ->
	mylist:uivalue(I);

uivalue(mylist2, I) ->
	mylist2:uivalue(I);

uivalue(Type, Id) ->
	helper:print(Type),
	helper:print(Id),
	throw(uivalue_does_not_exist).

uivalue(Fields) when is_list(Fields) ->
	lists:foldl(fun(#field {id=I, type=T, subfields=Sf} = F, Acc) ->
		case Sf of
			undefined ->
				V = fields:uivalue(T, I),
				case V of
					undefined -> Acc;
					_ -> Acc ++ [F#field {uivalue=V}]
				end;
			_ -> Acc ++ [F#field {subfields=uivalue(Sf)}]
		end
	end, [], Fields).

todb(Fields) when is_list(Fields) ->
	lists:foldl(fun(F, Acc) ->
		Acc ++ [todb(F#field.type, F#field.uivalue)]
	end, [], Fields).

todb(_, undefined) ->
	undefined;

todb(password_bcrypt, V) ->
	PasswordHash = try
		helper:state(password_bcrypt)
	catch
		_:_ -> undefined
	end,
	case PasswordHash of
		undefined ->
			{ok, Salt} = bcrypt:gen_salt(),
			{ok, Hash} = bcrypt:hashpw(V, Salt),
			helper:l2b(Hash);
		V -> 
			helper:l2b(V)
	end;

todb(textbox, V) ->
	helper:l2b(V);

todb(hidden, V) ->
	helper:l2b(V);

todb(primarykey, V) ->
	helper:l2b(V);

todb(foreignkey, V) ->
	helper:l2b(V);

todb(address, V) ->
	todb(V);

todb(textarea, V) ->
	helper:l2b(V);

todb(html, V) when is_binary(V) ->
	V;

todb(html, V) ->
	helper:l2b(V);

todb(dropdown, V) ->
	helper:l2b(V);

todb(date, V) ->
	helper:l2b(V);

todb(time, V) ->
	helper:l2b(V);

todb(fixed, V) ->
	helper:l2b(V);

todb(attachment, V) ->
	attachment:todb(V);

todb(createdby, V) ->
	helper:l2b(V);

todb(createdon, V) ->
	helper:l2b(V);

todb(mylist, V) ->
	mylist:todb(V);

todb(mylist2, V) ->
	mylist2:todb(V);

todb(_, _) ->
	throw(todb_does_not_exist).

toui(textbox, V) ->
	helper:b2l(V);

toui(hidden, V) ->
	helper:b2l(V);

toui(primarykey, V) ->
	helper:b2l(V);

toui(foreignkey, V) ->
	helper:b2l(V);

toui(address, V) ->
	toui(textarea, V);

toui(textarea, V) ->
	helper:b2l(V);

toui(html, V) ->
	helper:b2l(V);

toui(dropdown, V) ->
	helper:b2l(V);

toui(date, V) ->
	helper:b2l(V);

toui(time, V) ->
	helper:b2l(V);

toui(password_bcrypt, V) ->
	helper:b2l(V);

toui(fixed, V) ->
	helper:b2l(V);

toui(attachment, V) ->
	attachment:toui(V);

toui(createdby, V) ->
	helper:b2l(V);

toui(createdon, V) ->
	helper:b2l(V);

toui(mylist, V) ->
	mylist:toui(V);

toui(mylist2, V) ->
	mylist2:toui(V);

toui(Type, V) ->
	helper:print(Type),
	helper:print(V),
	throw(toui_does_not_exist).

toui(Fields) when is_list(Fields) ->
	lists:foldl(fun(#field {type=T, subfields=Sf, dbvalue=V} = F, Acc) ->
		case Sf of
			undefined -> Acc ++ [F#field {uivalue=fields:toui(T, V)}];
			_ -> Acc ++ [F#field {subfields=toui(Sf)}]
		end
	end, [], Fields).

dbvalue(#field {type=fixed, uivalue=V}) -> helper:l2b(V);
dbvalue(F) -> throw(F).

% helpers
filter(Fields, Id) ->
	lists:filter(fun(F) ->
		F#field.id == Id
	end, Fields).

member(Fields, Id) ->
	case filter(Fields, Id) of
		[] -> false;
		_ -> true
	end.

find(Fields, Id) ->
	case toui(filter(Fields, Id)) of
		[Found] -> Found;
		_ -> undefined
	end.

delete(Fields, Id) ->
	case filter(Fields, Id) of
		[Field] -> lists:delete(Field, Fields);
		_ -> Fields
	end.

finduival(Fields, Id) ->
	case find(Fields, Id) of
		undefined -> [];
		F -> F#field.uivalue
	end.

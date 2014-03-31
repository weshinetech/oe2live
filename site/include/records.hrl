-define(CREATE, "create").
-define(EDIT, "edit").
-define(VIEW, "view").
-define(SEARCH, "search").
-define(YES, "yes").
-define(ACTIVE, "active").
-define(ADMIN, "admin").
-define(EXPORT, "export").
-define(DB_USERS, "u_").
-define(DB_QUESTIONS, "q_").
-define(YETTOSTART, "yettostart").
-define(COMPLETED, "completed").
-define(RELOGIN, "relogin").

-record(field, {
	id,
	baseid,
	type,
	postback,
	label,
	subfields,
	validators,
	uivalue,
	dbvalue,
	renderer
}).

-record(jevent, {
	id,
	mode,
	label,
	bind,
	ignore
}).

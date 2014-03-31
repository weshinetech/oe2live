-module(helper_options).
-compile(export_all).

none() -> [
	{[], locale:get(please_select)}
].

get(testsactive) ->
	none() ++ oe2tests:list_active();

get(Id) ->
	Options = lists:foldl(fun(I, Acc) ->
		Acc ++ [{helper:a2l(I), locale:get(I)}]
	end, [], options(Id)),
	none() ++ Options.

options(role) -> [
	admin, staff
];

options(academic_branch) -> [
	academic_branch_cs, academic_branch_entc, academic_branch_it
];

options(academic_course_year) -> [
	academic_course_year_1, academic_course_year_2,
	academic_course_year_3, academic_course_year_4
];

options(academic_semester) -> [
	academic_semester_1, academic_semester_2
];

options(teststatus) -> [
	active,
	scheduled,
	completed,
	retired
];

options(blood_group) -> [
	onegative, opositive, anegative, apositive, bnegative, bpositive,
	abnegative, abpositive, unknown
];

options(address_country) -> [
	india
];

options(qualification) -> [
	quali_below_grade_10th, quali_grade_10th, quali_grade_12th, quali_ba, quali_ma,
	quali_bcom, quali_mcom, quali_bsc, quali_msc, quali_bca, quali_mca, quali_be,
	quali_me, quali_phd
];

options(year) -> [
	'1981', '1982', '1983', '1984', '1985', '1986', '1987', '1988', '1989', '1990',
	'1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999', '2000',
	'2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010',
	'2011', '2012', '2013'
];

options(oeuserexamstate) -> [
	yettostart,
	active,
	relogin,
	completed
];

options(oeuseraddreason) -> [
	oeuseraddreason_missing,
	oeuseraddreason_sno_unassigned,
	oeuseraddreason_retest,
	other
];

options(test_agent_result_generate) ->
	yesno();

options(Other) ->
	helper:print(Other),
	throw(optionlist_not_found).

yesno() -> [
	yes, no
].

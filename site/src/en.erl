-module(en).
-compile(export_all).

get(unauthorised_title) -> "Unauthorised";
get(unauthorised_heading) -> "Unauthorised";
get(tokens_title) -> "Tokens";
get(tokens_heading) -> "Tokens";
get(candidate_search_title) -> "Search Candidate";
get(candidate_search_heading) -> "Search Candidate";
get(candidate_status_title) -> "Candidate Status";
get(candidate_status_heading) -> "Candidate Status";
get(report_generate_title) -> "Generate Report";
get(report_generate_heading) -> "Generate Report";
get(instructions_title) -> "Instructions";
get(instructions_heading) -> "Instructions";

get(create) -> "Create";
get(edit) -> "Edit";
get(view) -> "View";
get(show_create) -> "Show";

get(please_select) -> "Please Select";
get(scheduled) -> "Scheduled";
get(active) -> "Active";
get(completed) -> "Completed";
get(retired) -> "Retired";
get(yet_to_start) -> "Yet To Start";

get(login) -> "Sign In";
get(password_bcrypt) -> "Password";
get(username) -> "Username";
get(signin) -> "Sign In";
get(footer_copyright) -> "(c) WeShineTech Pvt. Ltd., Pune";

get(login_title) -> "Sign In";
get(login_heading) -> "Sign In";
get(profile_login_title) -> "Sign In";
get(profile_login_heading) -> "Sign In";


get(oeusercentercode) -> "Centre Code";
get(oeuserseatnumber) -> "Seat Number";
get(oeuserfullname) -> "Full Name";
get(oeuseraddname) -> "Additional Name";
get(oeuserlogintimes) -> "# of logins";
get(oeusertoken) -> "Token";
get(oeusertimeleftseconds) -> "Time (secs)";
get(oeuserscore) -> "Score";
get(oeuserstarttime) -> "Start Time";
get(oeuserendtime) -> "End Time";
get(oeuserexamstate) -> "Exam State";

get(oe2test_title) -> "Test";
get(oe2test_heading) -> "Test";
get(testname) -> "Name";
get(testdescription) -> "Description";
get(teststatus) -> "Status";
get(testsactive) -> "Active Tests";

get(optiona) -> "A";
get(optionb) -> "B";
get(optionc) -> "C";
get(optiond) -> "D";
get(optione) -> "E";

get(validator_required) -> "Required";
get(validator_length5) -> "Too short";
get(validator_length8) -> "Too short";

get(login_failed) -> "Login failed.";
get(login_failed_expired) -> "Login failed. Your test has ended.";

get(exam_title) -> "Online Examination";
get(exam_heading) -> "Online Examination";
get(exam_questions_list) -> "Questions";
get(exam_marker) -> "Bookmark";
get(exam_report_invalid) -> "Invalid";
get(exam_first) -> "First";
get(exam_previous) -> "Previous";
get(exam_next) -> "Next";
get(exam_last) -> "Last";
get(exam_clear_selection) -> "Clear";
get(exam_submit_test) -> "Submit";
get(exam_question_info_index) -> "Question #~p";
get(exam_question_info_marks) -> "~s marks";
get(exam_option_header) -> "Option";
get(exam_option_description) -> "Description";
get(exam_questions_list_table) -> "List of Question";
get(exam_submit_test_confirm) -> "Are you sure you want to submit this test? You will not be able to take this test again.";
get(exam_submit_test_confirm_yes) -> "Yes. Submit this test.";
get(exam_submit_test_confirm_no) -> "Cancel";
get(exam_submit_important) -> "IMPORTANT NOTICE";
get(exam_option_clear_success) -> "Cleared selection for question (~p)";
get(exam_option_clear_failed) -> "Could not clear selection for question (~p)";
get(exam_option_save_success) -> "Saved option (~s) for question (~p)";
get(exam_option_save_failed) -> "Could not save option for question (~p)";
get(exam_marker_save_failed) -> "Could not save marker for question (~p)";
get(exam_reported_save_failed) -> "Could not report question (~p)";
get(exam_timer_save_failed) -> "Could not save timer";

get(instructions) -> "Instructions";
get(instructions_click) -> "Instructions";
get(instructions_login) -> "Sign In";
get(instructions_sno) -> "No.";
get(instructions_section) -> "Category";
get(instructions_description) -> "Description";
get(instructions_section_session) -> "Session";
get(instructions_section_timer) -> "Timer";
get(instructions_section_browser) -> "Browser";
get(instructions_attempts) -> "In case of system issues, candidates will be allowed to re-login. However, the number of re-logins is restricted.";
get(instructions_refresh) -> "Refreshing page or closing the browser will log you out of the system. Do not refresh page or close the browser during the exam.";
get(instructions_submit) -> "Once you submit the exam you will not be able to login again. Your exam is considered completed. Therefore, please be very careful and submit the exam only when you are sure you want to end the exam.";
get(instructions_timer) -> "A timer will show you time remaining (In minutes only. Seconds will not be displayed.) for the exam.";
get(instructions_browsers) -> "Recommended browsers: Latest versions of Mozilla Firefox and Google Chrome.";

get(profile_login) -> "Admin Sign In";

get(msg_unquthorised) -> "Unauthorised access to module.";
get(msg_candidate_not_found) -> "Not found.";
get(msg_online_exam_system) -> "Online Examination System";
get(msg_index_welcome) -> "Welcome";
get(msg_total_questions) -> "Total Questions";
get(msg_attempted_questions) -> "Attempted Questions";
get(msg_unattempted_questions) -> "Un-attempted Questions";

get(admin_link_home) -> "Home";
get(admin_link_tokens) -> "Tokens";
get(admin_link_candidate_add) -> "Add Candidate";
get(admin_link_candidate_search) -> "Search Candidate";
get(admin_link_candidate_summary) -> "Status Summary";
get(admin_link_candidate_status) -> "Status List";
get(admin_link_report_generate) -> "Generate Report";
get(admin_link_logout) -> "Logout";

get(Other) ->
	helper:a2l(Other).

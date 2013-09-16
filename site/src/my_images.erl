-module (my_images).
-compile(export_all).
-include("records.hrl").
-include_lib("nitrogen_core/include/wf.hrl").

main() ->
	send(wf:path_info()).

send("/" ++ AttachmentName) ->
	try
		["wst", TestId | _] = string:tokens(AttachmentName, "."),
		Url = "http://" ++
			db:host() ++
			":" ++ helper:i2s(db:port()) ++ 
			"/" ++ ?DB_QUESTIONS ++ TestId ++ 
			"/" ++ "/images" ++ 
			"/" ++ wf:url_encode(AttachmentName),

		case ibrowse:send_req(Url, [], get, [],
			[{basic_auth, {db:user(), db:password()}}]) of
				{ok, _, _Headers, Data} ->
					wf:content_type("image"),
					wf:header("Content-Disposition", "filename=" ++ AttachmentName),
					Data;
				_ ->
					[]
		end
	catch
		_:_ -> []
	end.

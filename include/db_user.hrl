%%% db_user fields:

-define(id, <<"id"/utf8>>).
-define(email, <<"email"/utf8>>).
-define(password, <<"password"/utf8>>).
-define(fname, <<"fname"/utf8>>).
-define(lname, <<"lname"/utf8>>).
-define(created, <<"created"/utf8>>).
-define(updated, <<"updated"/utf8>>).

-define(all_fields, [?id, ?email, ?password, ?fname, ?lname, ?created, ?updated]).

%% ?created, ?updated are excluded from view_fields because they have 
%% calendar:datetime() format. There is an appropriate function to convert 
%% them, but I prefer to do it in mysql_protocol.erl code than in my own code.
%% But there is no time to patch mysql-otp, this is why dates are just excluded 
%% from view_fields in this example app.
%%
%% convert function:
%% datetime_to_rfc3999({{Year, Month, Day}, {Hour, Min, Sec}}) ->
%%     list_to_binary(io_lib:format(
%%         "~.4.0w-~.2.0w-~.2.0wT~.2.0w:~.2.0w:~.2.0w.0+00:00", 
%%         [Year, Month, Day, Hour, Min, Sec])).
-define(view_fields, [?id, ?email, ?fname, ?lname]). 

-define(register_fields, [?email, ?password, ?fname, ?lname]).

-define(login_fields, [?email, ?password]).
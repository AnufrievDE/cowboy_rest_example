-module(auth_helper).

-export([get_jwt_cookie/1, set_jwt_cookie/3]).

get_jwt_cookie(Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    lists:keyfind(<<"jwt">>, 1, Cookies).

set_jwt_cookie(Secret, Claims, Req) ->
    JWT = jwt_wrapper:create_jwt(Secret, Claims),
    cowboy_req:set_resp_cookie(<<"jwt">>, JWT, Req).
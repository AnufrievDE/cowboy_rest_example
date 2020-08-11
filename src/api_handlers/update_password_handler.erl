-module(update_password_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_accepted/2]).

-export([update_password_from_json/2]).

-import(auth_helper, [get_jwt_cookie/1]).
-import(json_helper, [read_and_check_body/3, reply_error/3]).

-include_lib("kernel/include/logger.hrl").
-include("db_user.hrl").

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

is_authorized(Req, #{secret := Secret} = State) ->
    case get_jwt_cookie(Req) of
        {_, JWT} ->
            case jwt_wrapper:validate_jwt(JWT, Secret, [?id]) of
                {true, #{?id := UserId}} ->
                    {true, Req, State#{user_id => UserId}};
                false ->
                    Details = <<"JWT expired or not correct. Please relogin.">>,
                    {stop, reply_error(403, Details, Req), State}
            end;
        false ->
            Details = <<"JWT authentication token not found. Please relogin.">>,
            {stop, reply_error(403, Details, Req), State}
    end.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, update_password_from_json}
    ], Req, State}.

%%% ------------------------------------------------------------------
update_password_from_json(Req, #{user_id := UserId} = State) ->
    VerifyFun = fun db_user:validate_field/2,
    case read_and_check_body(Req, [?password], VerifyFun) of
        {ok, #{?password := NewPassword}, UReq} ->
            case mysql_user:update_password(UserId, NewPassword) of
                {ok, UserId} ->
                    {true, UReq, State};
                {error, _Reason} ->
                    {stop, reply_error(500, <<"Internal error">>, UReq), State}
            end;
        {error, Replied} ->
            {stop, Replied, State}
    end.
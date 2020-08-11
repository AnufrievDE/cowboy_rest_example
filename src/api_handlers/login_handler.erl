-module(login_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_accepted/2]).

-export([login_from_json/2]).

-import(auth_helper, [get_jwt_cookie/1, set_jwt_cookie/3]).
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
            case jwt_wrapper:validate_jwt(JWT, Secret) of
                false ->
                    {true, Req, State};
                _ -> %% jwt correct: user already logged in -> stop
                    {stop, cowboy_req:reply(204, Req), State}
            end;
        false -> %% No jwt: any person is authorized for login
            {true, Req, State}
    end.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, login_from_json}
    ], Req, State}.

%%% ------------------------------------------------------------------
login_from_json(Req, #{secret := Secret} = State) ->
    VerifyFun = fun db_user:validate_field/2,
    case read_and_check_body(Req, ?login_fields, VerifyFun) of
        {ok, ValidMap, UReq} ->
            LoginParams = [maps:get(F, ValidMap) || F <- ?login_fields],
            case erlang:apply(mysql_user, get_id, LoginParams) of
                {ok, UserId} ->
                    Resp = set_jwt_cookie(Secret, #{?id => UserId}, UReq),
                    %% 204 will be returned
                    {true, Resp, State};
                {error, nomatch_password} ->
                    Details = <<"Password does not match.">>,
                    {stop, reply_error(401, Details, UReq), State};
                {error, not_found} ->
                    Details = <<"User with this email is not registered.">>,
                    {stop, reply_error(401, Details, UReq), State};
                {error, _Reason} ->
                    {stop, reply_error(500, <<"Internal error">>, UReq), State}
            end;
        {error, Replied} ->
            {stop, Replied, State}
    end.
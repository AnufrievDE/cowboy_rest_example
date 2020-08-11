-module(register_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).

-export([register_from_json/2]).

-import(auth_helper, [set_jwt_cookie/3]).
-import(json_helper, [read_and_check_body/3, reply/4, reply_error/3]).

-include_lib("kernel/include/logger.hrl").
-include("db_user.hrl").

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, register_from_json}
    ], Req, State}.

%%% ------------------------------------------------------------------
register_from_json(Req, #{secret := Secret, site_url := SiteUrl} = State) ->
    VerifyFun = fun db_user:validate_field/2,
    case read_and_check_body(Req, ?register_fields, VerifyFun) of
        {ok, ValidMap, UReq} ->
            RegParams = [maps:get(F, ValidMap) || F <- ?register_fields],
            case erlang:apply(mysql_user, register, RegParams) of
                {ok, #{?id := UserId} = User} ->
                    %% Authenticate right after registration:
                    Resp = set_jwt_cookie(Secret, #{?id => UserId}, UReq),
                    %% Set location header
                    BinUserId = integer_to_binary(UserId),
                    L = <<SiteUrl/bytes, "/users/", BinUserId/bytes>>,
                    Replied = reply(201, #{<<"location">> => L},  User, Resp),
                    {stop, Replied, State};
                {error, already_exist} ->
                    Details = <<"User with this email already registered">>,
                    {stop, reply_error(409, Details, UReq), State};
                {error, _Reason} ->
                    {stop, reply_error(500, <<"Internal error">>, UReq), State}
            end;
        {error, Replied} ->
            {stop, Replied, State}
    end.

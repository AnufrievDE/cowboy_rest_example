-module(users_handler).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([content_types_provided/2]).

-export([users_to_json/2]).

-import(auth_helper, [get_jwt_cookie/1]).
-import(json_helper, [data_object/1, reply_error/3]).

-include_lib("kernel/include/logger.hrl").
-include("db_user.hrl").

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

is_authorized(Req, #{secret := Secret} = State) ->
    case get_jwt_cookie(Req) of
        {_, JWT} ->
            case jwt_wrapper:validate_jwt(JWT, Secret) of
                {true, _JWT}  ->
                    {true, Req, State};
                false ->
                    Details = <<"JWT expired or not correct. Please relogin.">>,
                    {stop, reply_error(403, Details, Req), State}
            end;
        false ->
            Details = <<"JWT authentication token not found. Please relogin.">>,
            {stop, reply_error(403, Details, Req), State}
    end.

content_types_provided(Req, State) ->
	{[
        {<<"application/json">>, users_to_json}
	], Req, State}.

%%% ------------------------------------------------------------------
users_to_json(#{bindings := #{id := UserId}} = Req, State) ->
    case mysql_user:get(UserId) of
        {ok, User} ->
            {data_object(User), Req, State};
        {error, not_found} ->
            {stop, cowboy_req:reply(404, Req), State};
        {error, _Reason} ->
            {stop, reply_error(500, <<"Internal error">>, Req), State}
    end;
users_to_json(Req, State) ->
    case mysql_user:list() of
        {ok, Users} ->
            {data_object(#{<<"users">> => Users}), Req, State};
        {error, _Reason} ->
            {stop, reply_error(500, <<"Internal error">>, Req), State}
    end.
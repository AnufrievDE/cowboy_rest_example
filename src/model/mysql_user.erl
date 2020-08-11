-module(mysql_user).

-behavior(db_user).

-include("db_user.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    get/1,
    get_id/2,
    list/0,
    is_registered/1,
    register/4,
    update_password/2
    %% debug
    %,exec_sql_req/1
    %,exec_sql_req/2
    %,exec_sql_req/3
]).

%%%===================================================================
%%% Defines
%%%===================================================================
-define(hash_alg, sha3_512).
-define(duplicate_entry_code, 1062).

-define(get_req(Fields, ByField),
    <<"SELECT ", (request_fields(Fields))/bytes, " FROM users WHERE ",
        ByField/bytes, "=?"/utf8>>).

-define(list_req(Fields),
    <<"SELECT ", (request_fields(Fields))/bytes, " FROM users"/utf8>>).

-define(insert_req(Fields, FieldsToReturn),
    <<"INSERT INTO users(", (request_fields(Fields))/bytes, ")
       VALUES (", (request_params(length(Fields)))/bytes, ") 
       RETURNING ", (request_fields(FieldsToReturn))/bytes, ""/utf8>>).

-define(update_password_req,
    <<"UPDATE users SET password =? WHERE id =?"/utf8>>).

%%%===================================================================
%%% API
%%%===================================================================
get(Id) when is_integer(Id) ->
    do_get(?view_fields, ?id, [Id]).

get_id(Email, Password) when is_binary(Email), is_binary(Password) ->
    PHash = crypto:hash(?hash_alg, Password),
    case do_get([?id | ?login_fields], ?email, [Email]) of
        {ok, #{?password := PHash, ?id := Id}} ->
            {ok, Id};
        {ok, #{?password := _OtherPHash}} ->
            {error, nomatch_password};
        {error, Reason} ->
            {error, Reason}
    end.
    
list() ->
    Fields = ?view_fields,
    SqlReq = ?list_req(Fields),
    Res = exec_sql_req(SqlReq),
    ConvFun = convert_row_fun(Fields),
    convert(Res, ConvFun).

register(Email, Password, FName, LName) ->
    PHash = crypto:hash(?hash_alg, Password),
    Fields = ?register_fields,
    SqlReq = ?insert_req(Fields, ?view_fields),
    Res = exec_sql_req(SqlReq, [Email, PHash, FName, LName]),
    ConvFun = convert_row_fun(?view_fields),
    case convert(Res, ConvFun) of
        {ok, [User]} ->
            {ok, User};
        {error, {mysql, {?duplicate_entry_code, _, _Msg}}} ->
            {error, already_exist};
        {error, Reason} ->
            {error, Reason}
    end.

is_registered(Email) ->
    {error, not_found} /= do_get([?id], ?email, [Email]).

update_password(Id, Password) ->
    PHash = crypto:hash(?hash_alg, Password),
    Res = exec_sql_req(?update_password_req, [PHash, Id]),
    case convert(Res) of
        ok -> {ok, Id};
        {error, Reason} ->
            {error, Reason}
    end.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
do_get(Fields, ByField, Params) ->
    SqlReq = ?get_req(Fields, ByField),
    Res = exec_sql_req(SqlReq, Params),
    ConvFun = convert_row_fun(Fields),
    case convert(Res, ConvFun) of
        {ok, []} -> {error, not_found};
        {ok, [User]} -> {ok, User};
        {error, Error} -> {error, Error}
    end.

exec_sql_req(SqlReq) ->
    exec_sql_req(SqlReq, no_params).
exec_sql_req(SqlReq, Params) ->
    %% it is not good to retrieve main_pool here,
    %% but it allows to implement pure user behavior cheap enough.
    exec_sql_req(persistent_term:get(main_pool), SqlReq, Params).
exec_sql_req(Pool, SqlReq, Params) ->
    poolboy:transaction(Pool,
        fun(W) ->
            mysql:query(W, SqlReq, Params)
        end).

request_fields(Fields) ->
    <<Byte || Byte <- lists:join(<<",">>, Fields)>>.

request_params(N) when N > 0 ->
    <<Byte || Byte <- lists:join(<<",">>, lists:duplicate(N, <<$?>>))>>.

convert(Res) ->
    convert(Res, fun(Row) -> Row end).

convert({ok, _Cols, Rows}, RowFun) ->
    {ok, lists:map(RowFun, Rows)};
convert({error, {Code, SQLState, Msg}}, _RowFun) ->
    {error, {mysql, {Code, SQLState, Msg}}};
convert(Other, _RowFun) -> Other.

convert_row_fun(Fields) ->
    fun(Row) ->
        maps:from_list(lists:zip(Fields, Row))
    end.
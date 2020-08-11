-module(db_user).

-export([validate_field/2]).

-include("db_user.hrl").

%%%===================================================================
%%% Types
%%%===================================================================
-type user() :: map().
-type user_ret() :: {ok, user()}.
-type user_id_ret() :: {ok, UserId :: integer()}.
-type user_list_ret() :: list(user_ret()).
-type error_ret() :: {error, Reason :: term()}.

%%%===================================================================
%%% Callbacks
%%%===================================================================
-callback get(UserId :: integer()) ->
          user_ret() | error_ret().

-callback list() -> user_list_ret() | error_ret().

-callback is_registered(Email :: binary()) -> boolean().

-callback register(Email :: binary(), Password :: binary(),
                   FirstName :: binary(), LastName :: binary()) ->
          user_ret() | error_ret().

-callback get_id(Email :: binary(), Password :: binary()) ->
          user_id_ret() | error_ret().

-callback update_password(UserId :: integer(), Password :: binary()) ->
          user_id_ret() | error_ret().

%%%===================================================================
%%% Defines
%%%===================================================================
-define(email_regexp,
    <<"(?:[a-z0-9!#$%&'*+\\/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+\\/=?^_`{|}~-]+)*|"
      "\"(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21\\x23-\\x5b\\x5d-\\x7f]|"
        "\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*"
        "[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\\[(?:"
        "(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}"
    "(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:"
    "(?:[\\x01-\\x08\\x0b\\x0c\\x0e-\\x1f\\x21-\\x5a\\x53-\\x7f]|"
    "\\[\\x01-\\x09\\x0b\\x0c\\x0e-\\x7f])+)\\])">>).

-define(email_error, <<"Email must be in the correct format like simple@example.com">>).
-define(password_error, <<"Password must contain at least eight characters.">>).
-define(fname_error, <<"First name must contain at least three characters.">>).
-define(lname_error, <<"Last name must contain at least three characters.">>).

%%%===================================================================
%%% API
%%%===================================================================
validate_field(?email, Email) ->
    is_binary(Email) andalso
    case re:run(Email, ?email_regexp) of
        {match, _} -> true;
        nomatch -> false
    end orelse 
    {false, ?email_error};
validate_field(?fname, FName) ->
    validate_binary_size(FName, 3) orelse {false, ?fname_error};
validate_field(?lname, LName) ->
    validate_binary_size(LName, 3) orelse {false, ?lname_error};
validate_field(?password, Password) ->
    validate_binary_size(Password, 8) orelse {false, ?password_error}.

validate_binary_size(Field, MinNoBytes) ->
    is_binary(Field) andalso byte_size(Field) >= MinNoBytes.


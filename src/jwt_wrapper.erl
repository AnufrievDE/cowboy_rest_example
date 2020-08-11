-module(jwt_wrapper).

-export([create_jwt/2, validate_jwt/2, validate_jwt/3]).

-include_lib("jose/include/jose_jwt.hrl").

-define(jwt_ttl, 15 * 60).
-define(exp, <<"exp">>).
-define(default_claims, #{?exp => erlang:system_time(second) + ?jwt_ttl}).

create_jwt(Secret, Claims) ->
    JWK = #{
        <<"kty">> => <<"oct">>,
        <<"k">> => jose_jwa_base64url:encode(Secret)
    },
    JWS = #{
        <<"alg">> => <<"HS256">>
    },
    JWT = maps:merge(?default_claims, Claims),
    SignedJWT = jose_jwt:sign(JWK, JWS, JWT),
    {_AlgInfo, CompactSignedJWT} = jose_jws:compact(SignedJWT),
    CompactSignedJWT.

validate_jwt(CompactJWT, Secret) ->
    JWK = #{
        <<"kty">> => <<"oct">>,
        <<"k">> => jose_jwa_base64url:encode(Secret)
    },
    try jose_jwt:verify(JWK, CompactJWT) of
        {true, JWT, _JWS} ->
            not is_expired(JWT) andalso {true, JWT};
        False ->
            erlang:display("invalid jwt ret(should be - false):"),
            erlang:display(False),
            False
    catch
        _:_:_ ->
            false
    end.

validate_jwt(CompactJWT, Secret, ClaimsToReturn) ->
    case validate_jwt(CompactJWT, Secret) of
        false -> false;
        {true, JWT} -> {true, parse_for(JWT, ClaimsToReturn)}
    end.

is_expired(#jose_jwt{fields = #{?exp := DateSeconds}}) ->
    erlang:system_time(second) > DateSeconds.

parse_for(#jose_jwt{fields = Claims}, Keys) ->
    maps:with(Keys, Claims).
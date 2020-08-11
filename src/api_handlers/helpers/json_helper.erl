-module(json_helper).

-export([data_object/1, read_and_check_body/3, reply/3, reply_error/3]).
-export([reply/4]).

-define(default_headers, #{<<"content-type">> => <<"application/json">>}).

read_and_check_body(Req, Fields, CheckFun) when is_function(CheckFun) ->
    {ok, JsonBody, Req1} = cowboy_req:read_body(Req),
    try jiffy:decode(JsonBody, [return_maps]) of
        JsonMap ->
        ExpectedMap = maps:with(Fields, JsonMap),
        case Fields -- maps:keys(ExpectedMap) of
            [] -> 
                FieldErrors = maps:fold(
                    fun(K, V, Acc) ->
                        case CheckFun(K, V) of
                            true -> Acc;
                            false -> [{K, <<"Invalid field.">>} | Acc];
                            {false, Error} -> [{K, Error} | Acc]
                        end
                    end, [], ExpectedMap),
                case FieldErrors of
                    [] ->
                        {ok, ExpectedMap, Req1};
                    _ ->
                        {error, reply_error(422, FieldErrors, Req1)}
                end;
            AbsentFields ->
                FieldErrors = 
                    [{F, <<"Mandatory field absent.">>} || F <- AbsentFields],
                {error, reply_error(422, FieldErrors, Req1)}
            end
    catch
        _:_:_ ->
            {error, reply_error(415, <<"Invalid json.">>, Req1)}
    end.

error_object(Status, MapInput) when is_map(MapInput) ->
    MapInput#{<<"status">> => integer_to_binary(Status)},
    jiffy:encode(#{<<"errors">> => [MapInput]});
error_object(Status, BinInput) when is_binary(BinInput) ->
    error_object(Status, #{<<"details">> => BinInput});
error_object(Status, KVInput) when is_list(KVInput) ->
    error_object(Status, maps:from_list(KVInput)).

reply_error(Status, ErrorObjectInput, Req) ->
    ErrorObject = error_object(Status, ErrorObjectInput),
    do_reply(Status, #{}, ErrorObject, Req).

data_object(DataBody) ->
    jiffy:encode(#{<<"data">> => DataBody}).

reply(Status, DataBody, Req) ->
    reply(Status, #{}, DataBody, Req).
reply(Status, CustomHeaders, DataBody, Req) ->
    DataObject = data_object(DataBody),
    do_reply(Status, CustomHeaders, DataObject, Req).

do_reply(Status, CustomHeaders, EncodedJson, Req) ->
    Headers = maps:merge(CustomHeaders, ?default_headers),
    cowboy_req:reply(Status, Headers, EncodedJson, Req).
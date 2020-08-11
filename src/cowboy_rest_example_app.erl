%%%-------------------------------------------------------------------
%% @doc cowboy_rest_example public API
%% @end
%%%-------------------------------------------------------------------

-module(cowboy_rest_example_app).

-behaviour(application).

-export([start/2, stop/1]).

%-define(web_server_https_port, 8443).
-define(web_server_http_port, 8080).

start(_StartType, _StartArgs) ->
    case application:get_all_env() of
        [] ->
            {error, no_config};
        Args ->
            %% put main_pool in persistent term
            persistent_term:put(main_pool, get_main_pool_from_args(Args)),
            
            Host = proplists:get_value(cowboy_host, Args),
            Port = proplists:get_value(cowboy_port, Args, ?web_server_http_port),
            Secret = proplists:get_value(secret, Args),

            start_cowboy(Host, Port, Secret),
            cowboy_rest_example_sup:start_link(Args)
    end.

stop(_State) ->
    ok = cowboy:stop_listener(my_http_listener).

%% internal functions
start_cowboy(Host, Port, Secret) ->
    BinHost = unicode:characters_to_binary(Host),
    BinPort = integer_to_binary(Port),
    SiteURL = <<"http://", BinHost/bytes, $:, BinPort/bytes>>,
    BinSecret = unicode:characters_to_binary(Secret),
    Dispatch = cowboy_router:compile([
            {'_', [
                {"/", hello_handler,
                    #{secret => BinSecret}},
                {"/register", register_handler,
                    #{secret => BinSecret, site_url => SiteURL}},
                {"/login", login_handler, 
                    #{secret => BinSecret}},
                {"/users/[:id]", [{id, int}], users_handler,
                    #{secret => BinSecret}},
                {"/update_password", update_password_handler,
                    #{secret => BinSecret}}
            ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}
    }),
    
    % It should be https server. But for test purpose http is ok.
    %
    %{ok, _} = cowboy:start_tls(my_https_listener,
    %    [
    %        {port, Port},
    %        {certfile, "/path/to/certfile"},
    %        {keyfile, "/path/to/keyfile"}
    %    ],
    %    #{env => #{dispatch => Dispatch}}),
    ok.

get_main_pool_from_args(Args) ->
    Pools = proplists:get_value(pools, Args, []),
    {Pool, _, _} = hd(Pools), %% at least one pool should be configured
    Pool.
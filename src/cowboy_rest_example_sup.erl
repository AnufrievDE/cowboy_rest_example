%%%-------------------------------------------------------------------
%% @doc cowboy_rest_example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cowboy_rest_example_sup).

-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

init(Args) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},

    Pools = proplists:get_value(pools, Args, []),

    PoolboysSupSpec = 
        #{id => poolboys_sup,
          start => {poolboys_sup, start_link, [Pools]},
          type => supervisor,
          restart => permanent,
          shutdown => infinity},

    ChildSpecs = [],
    {ok, {SupFlags, [PoolboysSupSpec | ChildSpecs]}}.

%% internal functions
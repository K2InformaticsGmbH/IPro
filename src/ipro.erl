%%%-------------------------------------------------------------------
%% @doc ipro public API
%% @end
%%%-------------------------------------------------------------------

-module(ipro).
-behaviour(application).
-behaviour(supervisor).
-include("ipro.hrl").

%% application
-export([start/0, stop/0]). % APIs
-export([start/2, stop/1]). % callbacks

%% supervisor callback
-export([init/1]).

%%====================================================================
%% application API
%%====================================================================
start() ->
    application:ensure_all_started(?MODULE).
stop() -> application:stop(?MODULE).

%%====================================================================
%% application callbacks
%%====================================================================
start(_StartType, _StartArgs) ->
    supervisor:start_link({local, ipro_sup}, ?MODULE, {}).

stop(_State) -> ok.

%%====================================================================
%% supervisor callbacks
%%====================================================================
init({}) ->
    ?L("supervisor starting...~n"),
    {ok, {
        #{strategy => one_for_one, intensity => 5, period => 10},
        [#{id => ipro_worker,
           restart => permanent, shutdown => 5000, type => worker,
           modules => [ipro_worker],
           start => {
                gen_server, start_link, [
                    {local, ipro_worker},
                    ipro_worker, [], []
                ]
            }
        }]
    }}.

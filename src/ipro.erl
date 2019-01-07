%%%-------------------------------------------------------------------
%% @doc ipro public API
%% @end
%%%-------------------------------------------------------------------

-module(ipro).
-behaviour(application).
-behaviour(supervisor).
-behaviour(gen_server).

%% application
-export([start/0, stop/0]). % APIs
-export([start/2, stop/1]). % callbacks

%% supervisor/gen_server hybrid init callback
-export([init/1]).

%% gen_server callback
-export([handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% log macros
-define(
    L(_Format, _Args),
    io:format(
        "~n[~p:~p:~p] ~p "_Format,
        [?MODULE, ?FUNCTION_NAME, ?LINE, self() | _Args]
    )
).
-define(L(_String), ?L(_String, [])).

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
%% supervisor/gen_server hybrid init
%%====================================================================
init({}) ->
    ?L("supervisor starting...~n"),
    {ok, {
        #{strategy => one_for_one, intensity => 5, period => 10},
        [#{id => ?MODULE,
           start => {gen_server, start_link,
                        [{local, ipro_epmd},
                         ?MODULE, [],
                         []]},
           restart => permanent, shutdown => 5000, type => worker,
           modules => [?MODULE]}]
    }};
init([]) ->
    ?L(
        "gen_server starting...~n"
        "ERLSRV_SERVICE_NAME=~s~n"
        "ERLSRV_EXECUTABLE=~s~n"
        "ERL_EPMD_PORT=~s~n"
        "kernel env ~p~n"
        "sasl env ~p~n",
        [os:getenv("ERLSRV_SERVICE_NAME"),
         os:getenv("ERLSRV_EXECUTABLE"),
         os:getenv("ERL_EPMD_PORT"),
         application:get_all_env(kernel),
         application:get_all_env(sasl)]
    ),
    net_info(),
    process_flag(trap_exit, true),
    {ok, undefined}.

%%====================================================================
%% gen_server callbacks
%%====================================================================
handle_call(Request, _From, State) ->
    {stop, {unsupported, Request}, unsupported, State}.

handle_cast(Request, State) ->
    {stop, {unsupported, Request}, State}.

handle_info(Info, State) ->
    {stop, {unsupported, Info}, State}.

terminate(Reason, State) ->
    ?L("exit for : ~p~nState: ~p~n", [Reason, State]).

%%====================================================================
%% Internal functions
%%====================================================================
-define(D(_E),
    ?L("--- "??_E" --->~n~p~n<--- "??_E" ---~n", [_E])
).

net_info() ->
    ?D(application:get_all_env(ipro)),
    ?D(inet:gethostname()),
    ?D(inet:getifaddrs()),
    ?D(erl_epmd:names()),
    ?D(erl_epmd:names({127,0,0,1})).
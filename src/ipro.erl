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
        "[~p:~p:~p] ~p "_Format,
        [?MODULE, ?FUNCTION_NAME, ?LINE, self() | _Args]
    )
).
-define(L(_String), ?L(_String, [])).

%%====================================================================
%% application API
%%====================================================================
start() -> application:ensure_all_started(?MODULE).
stop() -> application:stop(?MODULE).

%%====================================================================
%% application callbacks
%%====================================================================
start(_StartType, _StartArgs) ->
    ErtsBinPath = os:getenv("ERTS_BIN"),
    if ErtsBinPath == false ->
            error(
                "ERTS_BIN environment variable MUST be defined and point to "
                "erts-X.Y/bin folder (which contains epmd.exe)"
            );
        true -> ok
    end,
    EpmdExe = os:find_executable("epmd", ErtsBinPath),
    if EpmdExe == false -> error({ErtsBinPath, "doesn't have epmd.exe"});
        true -> ok
    end,
    supervisor:start_link(
        {local, ipro_sup}, ?MODULE, {EpmdExe, "127.0.0.1", "7999"}
    ).

stop(_State) -> ok.

%%====================================================================
%% supervisor/gen_server hybrid init
%%====================================================================
init({EpmdExe, Address, Port}) ->
    ?L("supervisor starting...~n"),
    {ok, {
        #{strategy => one_for_one, intensity => 5, period => 10},
        [#{id => ?MODULE,
           start => {gen_server, start_link,
                        [{local, ipro_epmd},
                         ?MODULE,
                         [EpmdExe, Address, Port],
                         []]},
           restart => permanent, shutdown => 5000, type => worker,
           modules => [?MODULE]}]
    }};
init([EpmdExe, Address, Port]) ->
    ?L("gen_server starting with parameters:~n"
       "    EpmdExe : ~p~n"
       "    Address : ~p~n"
       "    Port    : ~p~n", [EpmdExe, Address, Port]),
    process_flag(trap_exit, true),
    EpmdPort = open_port(
        {spawn_executable, EpmdExe},
        [in, {line, 80},
         {args, ["-d", "-d", "-d", "-d", "-address", Address, "-port", Port]},
         exit_status, stderr_to_stdout, use_stdio, {parallelism, true}]
    ),
    if not is_port(EpmdPort) ->
        {stop, {"Failed to start", EpmdExe, EpmdPort}};
        true ->
            true = erlang:link(EpmdPort),
            {ok, _} = net_kernel:start(['ipro@127.0.0.1']),
            true = erlang:set_cookie(node(), ipro),
            ?L("~nNode    : ~p~n"
               "Cookie  : ~p~n",
                [node(), erlang:get_cookie()]),
            {ok, {Port, EpmdPort, []}}
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
handle_call(Request, _From, State) ->
    {stop, {unsupported, Request}, unsupported, State}.

handle_cast(Request, State) ->
    {stop, {unsupported, Request}, State}.

%%====================================================================
%% Internal functions
%%====================================================================
handle_info({'EXIT', Port, Reason}, {_, Port, _} = State) ->
    ?L("[error] ~p terminated. reason ~p~n", [Port, Reason]),
    {stop, {'EXIT', Reason}, State};
handle_info({Port, closed}, {_, Port, _} = State) ->
    ?L("[error] ~p closed~n", [Port]),
    {stop, closed, State};
handle_info({Port, {exit_status, Status}}, {_, Port, _} = State) ->
    if Status == 0 ->
            ?L("~p finished successfully~n", [Port]);
        true ->
            ?L("[error] ~p exit with status ~p~n", [Port, Status])
    end,
    catch erlang:port_close(Port),
    {stop, normal, State};
handle_info({EpmdPort, {data, {F, Line}}}, {Port, EpmdPort, Buf}) ->
    {noreply,
        if F =:= eol ->
                ?L("~s~n", [lists:reverse([Line|Buf])]),
                {Port, EpmdPort, []};
            true -> 
                {Port, EpmdPort, [Line | Buf]}
        end};
handle_info({Port, {data, Data}}, {_, Port, _} = State) ->
    ?L("~p~n", [Data]),
    {noreply, State};
handle_info({'EXIT', FromPid, Reason}, {_, Port, _} = State) ->
    ?L("~p sent ~p, dying...~n", [FromPid, Reason]),
    catch erlang:port_close(Port),
    {stop, {'EXIT', Reason}, State}.

terminate(Reason, {Port, EpmdPort, _} = State) ->
    ?L("~nexit for : ~p, state : ~p~n"
       "stopping distribution  : ~p~n"
       "shutting down epmd     : ~p~n"
       "killed epmd processes  : ~p~n",
       [Reason, State, net_kernel:stop(), catch erlang:port_close(EpmdPort),
       catch os:cmd("powershell.exe -File kill_epmd.ps1 " ++ Port)]).
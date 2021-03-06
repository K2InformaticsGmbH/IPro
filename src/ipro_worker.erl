%%%-------------------------------------------------------------------
%% @doc ipro_wordker process
%% @end
%%%-------------------------------------------------------------------
-module(ipro_worker).
-behaviour(gen_server).
-include("ipro.hrl").

%% gen_server callback
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {idle_check_period, error_check_period, other_nodes, service,
     			exe, epmd_port}).

-define(TCP_CONNECT_TIMEOUT, 10000). % 10 sec

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    process_flag(trap_exit, true),
	Service = os:getenv("ERLSRV_SERVICE_NAME"),
	Exe = os:getenv("ERLSRV_EXECUTABLE"),
	EpmdPort = list_to_integer(os:getenv("ERL_EPMD_PORT")),
	{ok, IdleCheckPeriod} = application:get_env(idle_check_period),
	{ok, ErrorCheckPeriod} = application:get_env(error_check_period),
	{ok, OtherNodes} = application:get_env(other_nodes),
    ?L(
        "~n====== [~p starting] ======~n"
        "    ERLSRV_SERVICE_NAME : ~s~n"
        "    ERLSRV_EXECUTABLE   : ~s~n"
        "    ERL_EPMD_PORT       : ~p~n"
        "    IdleCheckPeriod     : ~p~n"
        "    ErrorCheckPeriod    : ~p~n"
        "    OtherNodes          : ~p~n"
        "    kernel env          : ~p~n"
        "    sasl env            : ~p~n"
		"====================================~n",
        [?MODULE, Service, Exe, EpmdPort, IdleCheckPeriod, ErrorCheckPeriod,
		 OtherNodes, application:get_all_env(kernel),
		 application:get_all_env(sasl)]
    ),
    {ok, #state{idle_check_period = IdleCheckPeriod,
				error_check_period = ErrorCheckPeriod,
				other_nodes = OtherNodes, exe = Exe,
				service = Service, epmd_port = EpmdPort},
	 IdleCheckPeriod}.

handle_call(Request, _From, State) ->
    {stop, {unsupported, Request}, unsupported, State}.

handle_cast(Request, State) ->
    {stop, {unsupported, Request}, State}.

handle_info(timeout, #state{idle_check_period = IdleCheckPeriod,
							error_check_period = ErrorCheckPeriod,
							other_nodes = OtherNodes} = State) ->
	case [N || N <- OtherNodes, net_adm:ping(N) /= pong] of
		[] ->
			?L("all nodes ~p are reachable, next check after : ~p ms~n",
			   [OtherNodes, IdleCheckPeriod]),
			{noreply, State, IdleCheckPeriod};
		UnreachableNodes ->
			?L("=============== analyze_report ===============~n"),
			analyze_report(UnreachableNodes, State),
			?L("==============================================~n"),
			?E("~p nodes were un-reachable, next check after : ~p ms~n",
			   [UnreachableNodes, ErrorCheckPeriod]),
			{noreply, State, ErrorCheckPeriod}
	end;
handle_info(Info, State) ->
    {stop, {unsupported, Info}, State}.

terminate(Reason, State) ->
    ?L("exit for : ~p~nState: ~p~n", [Reason, State]).

%%====================================================================
%% Internal functions
%%====================================================================

analyze_report([], _) -> net_info();
analyze_report([Node|UnreachableNodes], #state{epmd_port = EpmdPort} = State) ->
	{Host, IpAddr} = split_host(Node),
	case gen_tcp:connect(IpAddr, EpmdPort, [inet], ?TCP_CONNECT_TIMEOUT) of
		{ok, Socket} ->
			?L("~s (~s) epmd is reachable at port ~p~n",
			   [Host, inet:ntoa(IpAddr), EpmdPort]),
			gen_tcp:close(Socket);
		{error, Error} ->
			?E("~s (~s) epmd is NOT reachable at port ~p, Error ~p~n",
			   [Host, inet:ntoa(IpAddr), EpmdPort, Error])
	end,
	analyze_report(UnreachableNodes, State).

net_info() ->
	{Host, IpAddr} = split_host(node()),
	?L("FQDN host (from node) ~s (~s)~n", [Host, inet:ntoa(IpAddr)]),
    ?D(inet:gethostname()),
    ?D(inet:getifaddrs()),
	case erl_epmd:names() of
		{error, Error} -> ?E("erl_epmd:names() : ~p~n", [Error]);
		_ -> ?D(erl_epmd:names())
	end,
    ?D(erl_epmd:names(IpAddr)).

split_host(Node) ->
	[_, Host] = string:tokens(atom_to_list(Node), "@"),
	{ok, IpAddr} = inet:getaddr(Host, inet),
	{Host, IpAddr}.

#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

-define(ERL_EPMD_PORT, 				"7999").
-define(CLUSTER_PORTRANGE_START, 	"8000").
-define(CLUSTER_PORTRANGE_END, 		"8020").

-define(DEFAULT, #{
	epmd_port => ?ERL_EPMD_PORT,
	cluster_portrange_start => ?CLUSTER_PORTRANGE_START,
	cluster_portrange_end => ?CLUSTER_PORTRANGE_END,
	node => "ipro@127.0.0.1",
	cookie => "ipro",
	log_file => "ipro_"?ERL_EPMD_PORT".log"
}).

main([]) -> main(?DEFAULT);
main(Args) when is_list(Args) -> main(args(Args));
main(Args) ->
	start(maps:merge(?DEFAULT, Args)).

args(Args) -> args(Args, #{}).
args([], M) -> M;
args(["-name", V | R], M) -> args(R, M#{node => V});
args(["-setcookie", V | R], M) -> args(R, M#{cookie => V});
args(["-epmd_log", V | R], M) -> args(R, M#{epmd_log => V});
args(["-epmd_port", V | R], M) -> args(R, M#{epmd_port => V}).

-define(R(_Format, _Args), ok = io:format(get(logFile), _Format, _Args)).
-define(R(_String), ?R(_String, [])).

-define(L(_Format, _Args),
	?R("[~p:~p] "_Format, [?FUNCTION_NAME, ?LINE | _Args])
).
-define(L(_String), ?L(_String, [])).

-define(E(_Format, _Args), ?L("[error] "_Format, _Args)).
-define(E(_String), ?E(_String, [])).

start(#{log_file := _LogFile} = Config) ->
	%{ok, LogFileHandle} = file:open(LogFile, [write]),
	%put(logFile, LogFileHandle),
	put(logFile, user),
	?L("starting...~n"),
	start(maps:without([log_file], Config));
start(#{epmd_port := Port} = Config) when is_list(Port) ->
	true = os:putenv("ERL_EPMD_PORT", Port),

	?L(
		"OS Environment set to:~n"
		" ERL_EPMD_PORT=~s~n",
		[os:getenv("ERL_EPMD_PORT")]
	),
	EpmdArgs = [
		"-d", "-d", "-d", "-d",
		"-port", Port
	],
    EpmdPort = open_port(
        {spawn_executable, os:find_executable("epmd.exe")},
        [in, stream, {args, EpmdArgs},
         exit_status, stderr_to_stdout, use_stdio, {parallelism, true}]
    ),
	if
		not is_port(EpmdPort) ->
			?E("failed to start EPMD~n");
		true ->
			?L("epmd started~n args ~p~n", [EpmdArgs]),
			put(epmdPort, EpmdPort),
			start(maps:without([epmd_port], Config))
	end;
start(#{node := Node, cookie := Cookie,
		cluster_portrange_start := PortRangeMin,
		cluster_portrange_end := PortRangeMax} = Config) ->
	ErlNodeArgs = [
		"-boot", "start_clean",
		"-name", Node, "-setcookie", Cookie,
		"-kernel", "inet_dist_listen_min", PortRangeMin,
		"-kernel", "inet_dist_listen_max", PortRangeMax
	],
    ErlNodePort = open_port(
        {spawn_executable, os:find_executable("werl.exe")},
        [{args, ErlNodeArgs}, exit_status, stderr_to_stdout, use_stdio,
		 {parallelism, true}]
    ),
	if
		not is_port(ErlNodePort) ->
			?E("failed to start erlang node ~n");
		true ->

			put(erlNodePort, ErlNodePort),
			?L("erlang node started~n args ~p~n", [ErlNodeArgs]),
			start(
				maps:without(
					[node, cookie, cluster_portrange_start,
					 cluster_portrange_end],
					Config
				)
			)
	end;
start(Config) when map_size(Config) == 0 ->
	?L("starting epmd event loggin...~n"),
	epmd_log_loop(get(epmdPort)),
	?L("stopped!~n");
start(Config) ->
	?E("bad config ~p~n", [Config]),
	%ok = flie:close(get(logFile)),
	true = port_close(get(erlNodePort)),
	true = port_close(get(epmdPort)).

epmd_log_loop(Port) ->
	receive
		{'EXIT', Port, Reason} ->
			?E("[epmd] ~p terminated. reason ~p~n", [Port, Reason]);
		{Port, closed} ->
			?E("[epmd] ~p closed~n", [Port]);
		{Port, {exit_status, 0}} ->
			?L("[epmd] ~p closed gracefully~n", [Port]);
		{Port, {exit_status, Status}} ->
			?E("[epmd] ~p exit with status ~p~n", [Port, Status]),
    		catch erlang:port_close(Port);
		{Port, {data, Data}} ->
			?R(Data),
			epmd_log_loop(Port)
	after 1000 ->
		epmd_log_loop(Port)
	end.
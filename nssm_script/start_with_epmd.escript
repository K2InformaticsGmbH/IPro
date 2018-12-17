#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable

main([]) -> main(["-config", "default.conf"]);
main(["-config", ConfigFile]) ->
	{ok, [Config]} = file:consult(ConfigFile),
	main(Config);
main(Args) when is_list(Args) -> main(args(Args));
main(Args) ->
	{ok, [Config]} = file:consult("default.conf"),
	start(maps:merge(Config, Args)).

args(Args) -> args(Args, #{}).
args([], M) -> M;
args(["-name", V | R], M) -> args(R, M#{node => V});
args(["-setcookie", V | R], M) -> args(R, M#{cookie => V});
args(["-epmd_log", V | R], M) -> args(R, M#{epmd_log => V});
args(["-epmd_port", V | R], M) -> args(R, M#{epmd_port => V}).

-define(R(_Format, _Args), ok = io:format(get(logFile), _Format, _Args)).
-define(R(_String), ?R(_String, [])).

-define(L(_Format, _Args),
	?R("~s [~p:~p] "_Format, [ts(), ?FUNCTION_NAME, ?LINE | _Args])
).
-define(L(_String), ?L(_String, [])).

-define(E(_Format, _Args), ?L("[error] "_Format, _Args)).
-define(E(_String), ?E(_String, [])).

start(#{log_file := LogFile} = Config) ->
	if LogFile == console ->
			put(logFile, user);
		true ->
			{ok, LogFileHandle} = file:open(LogFile, [write]),
			put(logFile, LogFileHandle)
	end,
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

	EpmdExe = os:find_executable("epmd.exe"),
	epmd_cleanup(EpmdExe, Port),

    EpmdPort = open_port(
        {spawn_executable, EpmdExe},
        [in, stream, {args, EpmdArgs},
         exit_status, stderr_to_stdout, use_stdio, {parallelism, true}]
    ),
	if
		not is_port(EpmdPort) ->
			?E("failed to start EPMD~n");
		true ->
			?L("epmd started~n args ~p~n", [EpmdArgs]),
			put(epmdExe, EpmdExe),
			put(epmdTcpPort, Port),
			put(epmdPort, EpmdPort),
			start(maps:without([epmd_port], Config))
	end;
start(#{node := Node, cookie := Cookie,
		cluster_portrange_start := PortRangeMin,
		cluster_portrange_end := PortRangeMax,
		ping_node := PingNode, ping_log_file := PingLogFile} = Config) ->
	catch file:delete("ping.beam"),
	{ok, ping} = compile:file("ping"),
	ErlNodeArgs = [
		"-boot", "start_clean",
		"-name", Node, "-setcookie", Cookie,
		"-kernel", "inet_dist_listen_min", PortRangeMin,
		"-kernel", "inet_dist_listen_max", PortRangeMax,
		"-pa", ".",
		"-eval", "ping:loop('"++PingNode++"', \""++PingLogFile++"\")."
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
					 cluster_portrange_end, ping_node, ping_log_file],
					Config
				)
			)
	end;
start(Config) when map_size(Config) == 0 ->
	?L("starting epmd event logging...~n"),
	ErlangPort = get(erlNodePort),
	EpmdPort = get(epmdPort),
	epmd_log_loop(EpmdPort, ErlangPort),
	catch erlang:port_close(ErlangPort),
	kill_epmd(get(epmdExe), get(epmdTcpPort)),
    catch erlang:port_close(EpmdPort),
	?L("stopped!~n");
start(Config) ->
	?E("bad config ~p~n", [Config]),
	LogFileHandle = get(logFile),
	if  LogFileHandle /= user ->
			ok = flie:sync(LogFileHandle),
			ok = flie:close(LogFileHandle);
		true -> ok
	end,
	true = port_close(get(erlNodePort)),
	true = port_close(get(epmdPort)).

epmd_log_loop(EpmdPort, ErlangPort) ->
	receive
		{'EXIT', EpmdPort, Reason} ->
			?E("[epmd] ~p terminated. reason ~p~n", [EpmdPort, Reason]);
		{EpmdPort, closed} ->
			?E("[epmd] ~p closed~n", [EpmdPort]);
		{EpmdPort, {exit_status, 0}} ->
			?L("[epmd] ~p closed gracefully~n", [EpmdPort]);
		{EpmdPort, {exit_status, Status}} ->
			?E("[epmd] ~p exit with status ~p~n", [EpmdPort, Status]);
		{EpmdPort, {data, Data}} ->
			?R(Data),
			epmd_log_loop(EpmdPort, ErlangPort);
		{'EXIT', ErlangPort, Reason} ->
			?E("[erl] ~p terminated. reason ~p~n", [ErlangPort, Reason]);
		{ErlangPort, closed} ->
			?E("[erl] ~p closed~n", [ErlangPort]);
		{ErlangPort, {exit_status, 0}} ->
			?L("[erl] ~p closed gracefully~n", [ErlangPort]);
		{ErlangPort, {exit_status, Status}} ->
			?E("[erl] ~p exit with status ~p~n", [ErlangPort, Status]);
		{ErlangPort, {data, Data}} ->
			?R(Data),
			epmd_log_loop(EpmdPort, ErlangPort)
	after 1000 ->
		epmd_log_loop(EpmdPort, ErlangPort)
	end.

epmd_cleanup(EpmdExe, Port) ->
	CheckEpmdCmd = lists:flatten(
		io_lib:format("~p -d -port ~s -names", [EpmdExe, Port])
	),
	EpmdCheckOutput = os:cmd(CheckEpmdCmd),
	case re:run(EpmdCheckOutput, "name ipro at port") of
		{match, _} -> error({"ipro already running", EpmdCheckOutput});
		nomatch ->
			case re:run(EpmdCheckOutput, "epmd: up and running") of
				{match, _} ->
					?L("~s~n", [EpmdCheckOutput]),
					kill_epmd(EpmdExe, Port);
				nomatch ->	all_good
			end
	end.

kill_epmd(EpmdExe, Port) ->
	KillEpmdCmd = lists:flatten(
		io_lib:format("~p -d -port ~s -kill", [EpmdExe, Port])
	),
	KillEpmdCmdOutput = os:cmd(KillEpmdCmd),
	case re:run(KillEpmdCmdOutput, "Killed") of
		nomatch ->
			error({"failed to kill epmd", KillEpmdCmdOutput, "on port", Port});
		{match, _} ->
			?L("Successfully killed epmd at ~s~n", [Port]),
			?L("waiting 1 second for cleanup...~n"),
			timer:sleep(1000)
	end.

ts() -> ts(calendar:now_to_local_time(erlang:timestamp())).
ts({{Year, Month, Day}, {Hour, Minute, Second}}) ->
	list_to_binary(
		io_lib:format(
			"~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
			[Year, Month, Day, Hour, Minute, Second]
		)
	).
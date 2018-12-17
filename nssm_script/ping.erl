-module(ping).

-export([loop/2]).

-define(L(_F, _A), io:format(get(logFileHandle), "~s [~p:~p:~p] "_F, [ts(), ?MODULE, ?FUNCTION_NAME, ?LINE | _A])).
-define(L(_S), ?L(_S, [])).

loop(PingNode, PingLogFile) when is_list(PingLogFile) ->
	if PingLogFile == "console" ->
			put(logFileHandle, user);
		true ->
			{ok, PingLogFileHandle} = file:open(PingLogFile, [write]),
			put(logFileHandle, PingLogFileHandle)
	end,
	loop(PingNode).

loop(PingNode) ->
	case catch net_adm:ping(PingNode) of
		pong ->
			?L("~p is reachable, checking in 10 seconds~n", [PingNode]),
			timer:sleep(10000);
		pang ->
			?L("~p is NOT reachable, checking in 1 seconds~n", [PingNode]),
			?L("erl_epmd:names() ~p~n",	[erl_epmd:names()]),
			?L("inet:getifaddrs() ~p~n", [inet:getifaddrs()]),
			string:atom_to_list(PingNode)
			timer:sleep(1000);
		Error ->
			LogFileHandle = get(logFileHandle),
			if LogFileHandle /= user -> file:clode(LogFileHandle);
				true -> ok
			end,
			error(Error)
	end,
	loop(PingNode).

ts() -> ts(calendar:now_to_local_time(erlang:timestamp())).
ts({{Year, Month, Day}, {Hour, Minute, Second}}) ->
	list_to_binary(
		io_lib:format(
			"~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B",
			[Year, Month, Day, Hour, Minute, Second]
		)
	).
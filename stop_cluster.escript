#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name stop@127.0.0.1 -setcookie test

main([]) ->
	FirstNode = 'a@127.0.0.1',
	case net_adm:ping(FirstNode) of
		pong ->
			Cluster = [FirstNode | rpc:call(FirstNode, erlang, nodes, [])] -- [node()],
			io:format("stopping ~p~n", [Cluster]),
			rpc:multicall(Cluster, init, stop, []);
		_ -> halt(1)
	end.

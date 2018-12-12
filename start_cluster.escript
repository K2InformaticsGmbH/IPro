#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -name start@127.0.0.1 -setcookie test

main([NodeCountStr]) ->
	start_werls(list_to_integer(NodeCountStr)),
	io:format(
		"start complete : ~p of ~s nodes are up~n",
		[length(nodes()), NodeCountStr]
	),
	rpc:multicall(code, load_file, [test]),
	io:format(
		"mnesia:system_info:~n~p~n",
	 	[rpc:multicall(mnesia, system_info, [all])]
	);%,
	%rpc:multicall(test, start, []);
main([]) ->
	halt(1).

werl_cmds(Node) ->
	lists:flatten(
		["start werl.exe -pa _build/default/lib/ipro/ebin"
		 " -name ", Node,
		 " -setcookie test",
		 " -mnesia schema_location ram",
		 " -eval \"[net_adm:ping('", atom_to_list(node()), "')]\""]
	).

start_werls(0) -> all_started;
start_werls(NodeCount) when NodeCount > 0 ->
	Node = [$a + (NodeCount - 1) | "@127.0.0.1"],
	Cmd = werl_cmds(Node),
	[] = os:cmd(Cmd),
	wait_for_node(Node),
	start_werls(NodeCount - 1).

wait_for_node(Node) when is_list(Node) ->
	io:format("waiting for ~s...", [Node]),
	wait_for_node(list_to_atom(Node));
wait_for_node(Node) when is_atom(Node) ->
	case net_adm:ping(Node) of
		pong -> io:format("joined!~n");
		_ ->
			io:format("."),
			timer:sleep(1000),
			wait_for_node(Node)
	end.
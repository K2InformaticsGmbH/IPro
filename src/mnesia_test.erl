-module(test).
-behavior(gen_server).

-on_load(init/0).

-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

init() ->
	ok = application:load(mnesia),
	ok = application:set_env(mnesia, extra_db_nodes, nodes() -- ['start@127.0.0.1']),
	ok = mnesia:start(),
	io:format("mnesia started~n"),
	ok.

start() ->
	{ok, P} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
	io:format("test process started at ~p~n", [P]).

init([]) ->
	io:format("[~p:~p:~p]~n", [?MODULE, ?FUNCTION_NAME, ?LINE]),
	{ok, _} = mnesia:subscribe(system),
	{ok, []}.

handle_call(Request, _From, State) ->
	{stop, call_unimplimented, {badcall, Request}, State}.

handle_cast(Request, State) ->
	{stop, {cast_unimplimented, Request}, State}.

handle_info(Info, State) ->
	io:format("msg: ~p~n", [Info]),
	{noreply, State}.

terminate(Reason, State) ->
	io:format(
		"terminating... reason : ~p~n"
		"state : ~p~n",
		[Reason, State]
	).
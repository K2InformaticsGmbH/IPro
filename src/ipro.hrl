-ifndef(_IPRO_HRL_).
-define(_IPRO_HRL_, true).

% timestamp string
-define(T,
	(fun() ->
		{_, _, MicroSecs} = Now = os:timestamp(),
		SecFrac = MicroSecs / 1000000,
		{{Year, Month, Day},
		{Hour, Minute, Second}} = calendar:now_to_local_time(Now),
		lists:flatten(
			io_lib:format(
				"~4..0B.~2..0B.~2..0B ~2..0B:~2..0B:~9.6.0f",
				[Year, Month, Day, Hour, Minute, Second+SecFrac]
			)
		)
	end)()
).

%% log macros
-define(
    L(_Format, _Args),
    io:format(
        "~n~s [~p:~p:~p] ~p "_Format,
        [?T, ?MODULE, ?FUNCTION_NAME, ?LINE, self() | _Args]
    )
).
-define(L(_String), ?L(_String, [])).

-define(D(_E),
    ?L("~n--- "??_E" --->~n~p~n<--- "??_E" ---~n", [_E])
).

-define(E(_Format, _Args), ?L("[ERROR] "_Format, _Args)).
-define(E(_String), ?E(_String, [])).

-endif. % _IPRO_HRL_
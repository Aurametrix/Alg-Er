test() ->
    Pid = spawn(fun universal_server/0),
    Pid ! {become, fun factorial_server/0},
    Pid ! {self(), 50},
    receive
        X -> X
    end.

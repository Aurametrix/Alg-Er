factorial_server() ->
    receive
       {From, N} ->
           From ! factorial(N),
           factorial_server()
    end.

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

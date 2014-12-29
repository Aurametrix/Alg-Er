universal_server() ->
    receive
       {become, F} ->
           F()
    end.

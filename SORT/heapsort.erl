%% Isaac Gouy's code of 2002
%%
%% Quick and Dirty transliteration from the Mercury solution
%% with +1 adjustment for array indexes. 
%% Mercury uses 0..N-1 and Erlang uses 1..N
%%
%% Usage: start from command line with
%%     erlc heapsort.erl
%%     erl -noinput -s heapsort main 10000

-module(heapsort). 
-export([main/1]). 


random_heap(I, Seed, H) ->
    if 
        I < size(H) -> 
            {NextSeed, R} = gen_random(Seed),
            random_heap(I+1, NextSeed, up_heap(I, R, H));
        true -> H
    end.


up_heap(N, Y, H) ->
    HalfN = N div 2,
    X = element(HalfN+1, H), %%%% +1
    Condition = 0 < N andalso X < Y,
    if 
        Condition -> up_heap(HalfN, Y, setelement(N+1, H, X)); %%%% +1
        true -> setelement(N+1, H, Y) %%%% +1
    end.


heapsort(0, H) -> H;
heapsort(N, H) -> heapsort(N-1, remove_greatest(N, H)).


remove_greatest(N, H) ->
    X = element(0+1, H), %%%% +1
    Y = element(N+1, H), %%%% +1
    down_heap(0, N-1, Y, setelement(N+1, H, X)). %%%% +1


down_heap(I, N, X, H) -> 
    L = I + I + 1,
    R = L + 1,
    if
        N < L -> 
            setelement(I+1, H, X); %%%% +1
        true ->
            Condition = R < N andalso element(R+1, H) > element(L+1, H), %%%% +1
            J = if 
                   Condition -> R;
                   true -> L
                end,
            Y = element(J+1, H),
            if
                X > Y -> setelement(I+1, H, X); %%%% +1
                true -> down_heap(J, N, X, setelement(I+1, H, Y)) %%%% +1
            end
    end.


gen_random(Seed) ->
    IM = 139968, IA = 3877, IC = 29573,
    S = ((Seed * IA) + IC) rem IM,
    {S, S/IM}.


main([Arg]) ->
    N = list_to_integer(atom_to_list(Arg)),
    Seed = 42,
    RandomHeap = random_heap(0, Seed, erlang:make_tuple(N, 0.0)),
    SortedHeap = heapsort(N-1, RandomHeap),
    io:fwrite("~.10f~n", [element(N, SortedHeap)]),            
    halt(0).

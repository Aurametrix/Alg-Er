-module(mergesort).
-export(msort/2, msort_lte/1, msort_gte/1).

split(Ls) ->
    split(Ls, Ls, []).


split([], Ls1, Ls2) ->
    {reverse(Ls2) , Ls1};


split([_], Ls1, Ls2) ->
    {reverse(Ls2) , Ls1};

split([_,_|TT], [Ls1_H | Ls1_T], Ls2) ->

    split(TT, Ls1_T, [Ls1_H | Ls2]).

merge(_, [], Ls2) ->
    Ls2;

merge(_, Ls1, []) ->
    Ls1;

merge(Rel, [H1|T1], [H2|T2]) ->
    case Rel(H1, H2) of
	true ->
	    [H1 | merge(Rel, T1, [H2|T2])];
	false ->
	    [H2 | merge(Rel, [H1|T1], T2)]
    end.

msort(_, []) ->
    [];

msort(_, [H]) ->
    [H];

msort(Rel, Ls) ->
    {Half1 , Half2} = split(Ls),
    merge(Rel, msort(Rel, Half1), msort(Rel, Half2)).

% Parameterize msort with commonly-used predicates
lte(X, Y) ->
    (X < Y) or (X == Y).


gte(X, Y) ->
    (X > Y) or (X == Y).


msort_lte(Ls) ->
    msort(fun lte/2, Ls).


msort_gte(Ls) ->
    msort(fun gte/2,
	  Ls).


pMergeSort(L) when length(L) == 1 -> L;
pMergeSort(L) when length(L) > 1 ->
    {L1, L2} = lists:split(length(L) div 2, L),
    spawn(mergesort, pMergeSort2, [L1, self()]),
    spawn(mergesort, pMergeSort2, [L2, self()]),
    mergeResults([]).
 
pMergeSort2(L, Parent) when length(L) == 1 -> Parent ! L;
pMergeSort2(L, Parent) when length(L) > 1 ->
    {L1, L2} = lists:split(length(L) div 2, L),
    spawn(mergesort, pMergeSort2, [L1, self()]),
    spawn(mergesort, pMergeSort2, [L2, self()]),
    Parent ! mergeResults([]).

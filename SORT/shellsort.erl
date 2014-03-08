%% R.C.
%%
%% Simple, one process version
%% Takes advantage of built-in lists:split and lists:merge functions
mergeSort(L) when length(L) == 1 -> L;
mergeSort(L) when length(L) > 1 ->
    {L1, L2} = lists:split(length(L) div 2, L),
    lists:merge(mergeSort(L1), mergeSort(L2)).

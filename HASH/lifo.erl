-module(lifo).
-export([new/0]).
-export([append/2]).
-export([peek/1]).
-export([next/1]).
 
new() ->
    {[], []}.
 
append(V, {Old, New}) ->
    {Old, [V|New]}.
 
peek({[], []}) ->
    undefined;
peek({[H|_], _}) ->
    H;
peek({[], Old}) ->
    hd(lists:reverse(Old)).
 
next({[], []}) ->
    undefined;
next({[_|T], New}) ->
    {T, New};
next({[], New}) ->
    next({lists:reverse(New), []}).
 

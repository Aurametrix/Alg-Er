/*
*
* Keep the queue in a list and take elements from the head and add new elements to the end
*
* new() -> queue()  Returns an empty queue.
*
*/


new(Max) when is_integer(Max), Max > 0 -> {0,Max,[]}.   %Length, Max and Queue list

take({L,M,[H|T]}) -> {H,{L-1,M,T}}.

add(E, {L,M,Q}) when L < M ->
    {L+1,M,Q ++ [E]};                                   %Add to end of list
add(E, {M,M,[H|T]}) -
    {M,M,T ++ [E]}.                                     %Add to end of list

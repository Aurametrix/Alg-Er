 -module(math).
 -export([factorial/1]).

 factorial(0) ->
   1;
 factorial(X) when X > 0 ->
   X * factorial(X-1).


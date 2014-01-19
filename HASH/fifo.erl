-module(fifo).
-export([new/0, loop/0, push/2, pop/1]).

new() ->
spawn(?MODULE, loop, []).

loop() ->
receive
{Pid,pop} ->
Pid ! {self(),receive {push,X} ->
X
end),
loop()
end.

push(Fifo,X) ->
Fifo ! {push,X},
X.
 
pop(Fifo) ->
Fifo ! {self(),pop},
receive {Fifo,X} ->
X
end.

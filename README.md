Erlang
======

for building massively scalable highly-available soft real-time systems. 
Runtime system has built-in support for concurrency, distribution and fault tolerance.

Usage
$ erlc hello.erl 
$ erl -noshell -s hello start -s init stop


#### OTP-21 highlights

    example({ok, Val}) -> {ok, Val}.
to

    example({ok, Val} = Tuple) -> Tuple.

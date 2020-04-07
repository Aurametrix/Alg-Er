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


[OTP on Open BSD](http://blog.obligd.com/posts/erlang-otp-on-openbsd.html)


#### 

+ [Erlang: from Hype to 2019](https://ferd.ca/ten-years-of-erlang.html)

+ [BEAM](https://www.youtube.com/watch?v=JvBT4XBdoUE), The Soul of Erlang and Elixir  - talk by Saša Jurić at GOTO 2019 (April 29); [slides](https://gotochgo.com/2019/sessions/712)

+ [A History of Erlang](https://dl.acm.org/doi/abs/10.1145/1238844.1238850)

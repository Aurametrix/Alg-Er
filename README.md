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
+ [Hard Things to Get Right](https://adoptingerlang.org/docs/development/hard_to_get_right/)

+ [BEAM](https://www.youtube.com/watch?v=JvBT4XBdoUE), The Soul of Erlang and Elixir  - talk by Saša Jurić at GOTO 2019 (April 29); [slides](https://gotochgo.com/2019/sessions/712)

+ [A History of Erlang](https://dl.acm.org/doi/abs/10.1145/1238844.1238850)

+ [BEAM](https://blog.erlang.org/a-brief-BEAM-primer/) - virtual machine that executes user code in the Erlang Runtime System (ERTS)

+ [Detecting dead code](https://github.com/AdRoll/rebar3_hank) - [blog about the code](https://tech.nextroll.com/blog/dev/2021/01/06/erlang-rebar3-hank.html)
+ [Elixir w/Witchcraft](https://blog.appsignal.com/2022/02/08/functional-programming-in-elixir-with-witchcraft.html)
+ [Gleam](https://gleam.run/) - written in Rust, running on the battle-tested Erlang virtual machine
+ [The Soul of Erlang](https://youtu.be/JvBT4XBdoUE) - 2019 talk by Sasa Juric
+ [Zotonic](http://zotonic.com/) - real-time web framework : [newest v1](https://test.zotonic.com/)
+ [LISP-flavored Erlang](https://lfe.io/)
+ [eqWAlizer](https://github.com/WhatsApp/eqwalizer) - a type-checker for Erlang

+ [Lunatic](https://github.com/lunatic-solutions/lunatic) -  – An Erlang Inspired WebAssembly Platform. demos:
    + [turns any command line application into a web server endpoint](https://lunatic.solutions/run/)
    + [telnet chat server written in Rust using actor-based architecture](https://github.com/lunatic-solutions/chat)

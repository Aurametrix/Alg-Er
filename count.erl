-module(count).

-doc([{author, 'unimportant'},
      {title, "just a count test"},
      {keywords,[count,test,erlang]},
      {date, 12022013}]).

%% count the number of a's in a file

-export([file/1]).

file(F) ->
    lists:foldl(fun($a,N) -> N + 1;
           (_, N) -> N
            end,
        0,
        binary_to_list(element(2, file:read_file(F)))).


%% count:file("count.erl))

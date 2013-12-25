-module(test_qsort).
-export([start/0]).

start() ->
    io:format("Testing qsort...~n"),
    test_numeric_sort({qsort, qsort1}),
    test_string_sort({qsort, qsort1}),
    test_numeric_sort({qsort, qsort2}),
    test_string_sort({qsort, qsort2}),
    test_numeric_sort({qsort, qsort3}),
    test_string_sort({qsort, qsort3}),
    halt().

test_numeric_sort({M,F}) ->
    Stimulus = [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3],
    Response = [1,1,2,3,3,3,4,5,5,5,6,7,8,9,9,9],
    test_sort({M, F}, "numeric sort", Stimulus, Response).
    
test_string_sort({M, F}) ->
    Stimulus = ["bob","alice","barry","zoe","charlotte","fred","marvin"],
    Response = ["alice","barry","bob","charlotte","fred","marvin","zoe"],
    test_sort({M, F}, "string sort", Stimulus, Response).
    
test_sort({M, F}, Testname, S, R) ->
    FunName = erlang:atom_to_list(F),
    ModName = erlang:atom_to_list(M),
    Result =  M:F(S),
    if
        R == Result -> 
            io:format("~s:~s - passed - ~s.~n", [ModName, FunName, Testname]);
        true -> 
            io:format("~s:~s - failed - ~s.~n", [ModName, FunName, Testname])
    end.
% Modules can be compiled at the command line, using the erlc compiler: erlc qsort.erl 
% erl -run qsort

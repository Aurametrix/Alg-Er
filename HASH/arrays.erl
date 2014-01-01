array:new().
array:size(A).
A = array:new(5).
array:size(A).
B = array:set(5, "a", A).
array:size(B).
array:set(4,"a",A).
C = array:set(4, "b", B).
A = array:new(10, {fixed, false}).
A = array:from_list(["hello", "world", "erlang"]).
P = array:from_orddict([{1, "hello"},{5, "world"}]).
Q = array:from_orddict([{1, "hello"},{5, "world"}], -1).

array:to_list(Q).
array:to_list(P).
array:sparse_to_list(P).
array:sparse_to_orddict(Q).
B = array:from_list([1,2,3,undefined,undefined,5,undefined]).
array:sparse_map(fun(Index, Value) -> Value * 2 end, B).

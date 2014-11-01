% FILE:    exprParser.erl
% PURPOSE: parse an expression
% AUTHOR:  mckeeman@mathworks.com
%
% EXAMPLES: 
%   exprParser:parse("")                 ""
%   exprParser:parse("x")                "x"
%   exprParser:parse("(x)")              "x"
%   exprParser:parse("y*(x-1)")          {mpy,"y",{sub,"x",1}}
%   exprParser:parse("y*(3-1)")          {add,"y",2}
%
% CFG:
%   expr   = sum 
%   sum    = term 
%   sum    = sum '+' term 
%   sum    = sum '-' term
%   term   = term '*' factor
%   term   = term '/' factor 
%   factor = 0-9
%   factor = a-z 
%   factor = '(' sum ')'
%
% The left recursion in the CFG is turned into an iteration
% in the regular expression grammar.
% REG:
%   expr   = sum 
%   sum    = term ('+' term | '-' term)*
%   term   = factor ('*' factor | '/' factor)*
%   factor = 0-9 | a-z | '(' sum ')'
%
% METHOD: 
%   The parser takes each character in turn.
%   The result is an Abstract Syntax Tree.
%   Constant nodes are folded.
% NODE:
%   {op,node, node} or id or int.
% PARSE TREE:
%   nested nodes
%
% IMPLEMENTATION:
%   The typical parse function f has the form
%      {Ast, text} = f(Ast, text)
%   where at each step the Ast grows, the text becomes shorter.
%   Syntax errors yield cryptic failure messages.

-module(exprParser).
-export([parse/1]).

parse("")  -> "";
parse(Txt) -> 
  {Ast, []} = expr(Txt),
  Ast.

expr(Txt) -> sum(Txt).

% sum = term ('+' term | '-' term)*
sum(Txt) -> 
  {Ast, Txt1} = term(Txt),                       % at least one term
  addops(Ast, Txt1).                             % the rest, if any

addops(Ast, []) -> {Ast, []};
addops(Ast, [$+|Txt]) ->                         % +
  {Ast1, Txt1} = term(Txt),
  Ast2 = operate(add, Ast, Ast1),
  addops(Ast2, Txt1);  
addops(Ast, [$-|Txt]) ->                         % -
  {Ast1, Txt1} = term(Txt),
  Ast2 = operate(sub, Ast, Ast1),
  addops(Ast2, Txt1);
addops(Ast, Txt) -> {Ast, Txt}.                  % no more to do

% term = factor ('*' factor | '/' factor)*
term(Txt) -> 
  {Ast, Txt1} = factor(Txt),                     % at least one factor
  mulops(Ast, Txt1).                             % the rest, if any

mulops(Ast, []) -> {Ast, []};
mulops(Ast, [$*|Txt]) ->                         % *
  {Ast1, Txt1} = factor(Txt),
  Ast2 = operate(mpy, Ast, Ast1),
  mulops(Ast2, Txt1);
mulops(Ast, [$/|Txt]) ->                         % /
  {Ast1, Txt1} = factor(Txt),
  Ast2 = operate(dvd, Ast, Ast1),
  mulops(Ast2, Txt1);
mulops(Ast, Txt) -> {Ast, Txt}.                  % no more to do

% factor = '(' sum ')' | a-z | 0-9
factor([$(|Txt]) ->                              % ( sum )
  {Ast, [$) | Txt1]} = sum(Txt),                 % discard )
  {Ast, Txt1};
factor([H|T]) when H>=$a, $z>=H -> {[H], T};     % lower case letters
factor([H|T]) when H>=$0, $z>=9 -> {H-$0, T}.    % digits
 
% build subexpression node or constant fold
operate(Op, V1, V2) ->
  if 
    is_number(V1) andalso is_number(V2) ->       % both args const
      case Op of                                 % Erlang arith
        add -> V1+V2;
        sub -> V1-V2;
        mpy -> V1*V2;
        dvd -> V1/V2
      end;
    true ->                                      % otherwise build node
      {Op, V1, V2}
  end.

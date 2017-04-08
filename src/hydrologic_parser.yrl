%%% -*- mode: erlang -*-

Header
  "% @hidden".

Nonterminals
  Pipeline
  Expressions Expression
  ExpressionContent
  IN OUT
  Function FunctionContent
  Fanin
  Merge.

Terminals
  pipe return
  identifier
  operator
  fun end mf
  in out
  integer float
  fanin
  merge.

Rootsymbol
  Pipeline.

Unary 300 Expression.

Pipeline -> Expressions : "[" ++ '$1' ++ "]".

Expressions -> Expression : '$1'.
Expressions -> Expression Expressions : '$1' ++ ", " ++ '$2'.

Expression -> pipe ExpressionContent : '$2'.
Expression -> pipe IN : '$2'.
Expression -> pipe OUT : '$2'.
Expression -> return in : in('$1', '$2').
Expression -> return : "return".

ExpressionContent -> identifier : identifier('$1').
ExpressionContent -> Function : '$1'.
ExpressionContent -> Merge : '$1'.

IN -> in : in('$1').
IN -> in OUT : in('$1', '$2').
IN -> in Fanin : in('$1', '$2').
IN -> in ExpressionContent : in('$1', '$2').

OUT -> ExpressionContent out : out('$1', '$2').

Fanin -> fanin : "fanin".
Fanin -> fanin integer : fanin('$2').

Function -> fun FunctionContent end : function('$2').
Function -> fun mf operator integer : funcall('$2', '$3', '$4').

FunctionContent -> operator : operator('$1').
FunctionContent -> operator FunctionContent : operator('$1') ++ '$2'.
FunctionContent -> identifier : identifier('$1').
FunctionContent -> identifier FunctionContent : identifier('$1') ++ '$2'.
FunctionContent -> integer : integer('$1').
FunctionContent -> integer FunctionContent : integer('$1') ++ '$2'.
FunctionContent -> float : to_float('$1').
FunctionContent -> float FunctionContent : to_float('$1') ++ '$2'.

Merge -> merge Function : "{merge, " ++ '$2' ++ "}".
% Merge -> merge MFA : "{merge, " ++ '$1' ++ "}".
%
% MFA ->  TODO

Erlang code.

operator({operator, _, ","}) ->
  ", ";
operator({operator, _, "->"}) ->
  " -> ";
operator({operator, _, Operator}) ->
  Operator.

identifier({identifier, _, Identifier}) ->
  Identifier.

integer({integer, _, Integer}) ->
  bucs:to_string(Integer).

to_float({float, _, Float}) ->
  bucs:to_string(Float).

in({in, _, Label}) ->
  Label.
in({in, _, Label}, Expression) ->
  "{" ++ Label ++ ", " ++ Expression ++ "}";
in({return, _, _}, {in, _, Label}) ->
  "{" ++ Label ++ ", return}".

out(Expression, {out, _, Label}) ->
  "{" ++ Expression ++ ", " ++ Label ++ "}".

fanin({integer, _, N}) when N == 0; N == 1 ->
  "{fanin, " ++ bucs:to_string(N) ++ "}".

function(Body) ->
  "fun" ++ Body ++ " end".

funcall({mf, _, MF}, {operator, _, "/"}, {integer, _, Arity}) ->
  "fun " ++ MF ++ "/" ++ bucs:to_string(Arity).


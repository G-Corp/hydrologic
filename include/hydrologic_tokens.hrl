
-define(is_digit(S), (S >= $0 andalso S =< $9)).
-define(is_upcase(S), (S >= $A andalso S =< $Z)).
-define(is_downcase(S), (S >= $a andalso S =< $z)).
-define(is_space(S), ((S == $\s) orelse (S == $\t))).
-define(is_cr(S), S == $\r).
-define(is_lf(S), S == $\n).

-define(is_identifier(S), (?is_upcase(S) orelse ?is_downcase(S))).
-define(is_identifier_part(S), (?is_identifier(S) orelse ?is_digit(S) orelse S == $_ orelse ?is_in_out(S))).

-define(is_op(S), (not (?is_digit(S) orelse ?is_upcase(S) orelse ?is_downcase(S) orelse ?is_space(S) orelse ?is_cr(S) orelse ?is_lf(S)))).
-define(is_in_out(T), T == $:).
-define(is_pipe_op(T), T == $|).
-define(is_return_op(T), T == $?).

% -define(dot_op(T), T == $.).
% -define(match_op(T), T == $=).
% -define(match_op2(T1, T2), T1 == $-, T2 == $>).
% -define(mult_op(T), T == $* orelse T == $/).
% -define(dual_op(T), T == $+ orelse T == $-).


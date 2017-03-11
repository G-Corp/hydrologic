-module(hydrologic_lib).

-export([parse_transform/2]).

parse_transform(Form, _Options) ->
  Specs = get_specs(Form),
  [{eof, L}|AST] = lists:reverse(Form),
  Fun = {function, L, hydrologic__types, 0, [{clause, L, [], [], [build_value(Specs, L)]}]},
  NewAST = lists:reverse([{eof, L + 1}, Fun|AST]),
  add_export(NewAST).

add_export([]) -> [];
add_export([{attribute, N, export, Exports}|Rest]) ->
  [{attribute, N, export, [{hydrologic__types, 0}|Exports]}|Rest];
add_export([Part|Rest]) ->
  [Part|add_export(Rest)].

get_specs(Form) ->
  get_specs(Form, []).

get_specs([], Acc) ->
  lists:reverse(Acc);
get_specs([{attribute, _N, spec, Spec}|Rest], Acc) ->
  get_specs(Rest, [spec_info(Spec)|Acc]);
get_specs([_|Rest], Acc) ->
  get_specs(Rest, Acc).

spec_info({{Name, Arity}, Funs}) ->
  {{Name, Arity}, get_type(Funs, undefined)}.

get_type([], Type) ->
  Type;
get_type([{type, _, 'fun',
          [_, Return]}|Rest], Type) ->
  get_type(Rest, type(Type, Return)).

type(Type, {type, _, tuple, [{atom, _, NewType}|_]}) when Type == NewType;
                                                          Type == undefined ->
  NewType;
type(Type, {type, _, union, Return}) ->
  case lists:usort([type(Type, R) || R <- Return]) of
    [Unique] -> Unique;
    _ -> error
  end;
type(Type, _) when Type == map;
                   Type == undefined ->
  map;
type(_, _) ->
  error.






build_value(X, N) when is_atom(X) ->
  build_atom(X, N);
build_value(X, N) when is_integer(X) ->
  build_integer(X, N);
build_value(X, N) when is_list(X) ->
  build_list(X, N);
build_value(X, N) when is_tuple(X) ->
  build_tuple(X, N).

build_atom(A, N) when is_atom(A) ->
  {atom, N, A}.

build_integer(I, N) when is_integer(I) ->
  {integer, N, I}.

build_list([], N) -> {nil, N};
build_list(L, N) ->
  [T|H] = lists:reverse(L),
  E = {cons, N, build_value(T, N), {nil, N}},
  do_build_list(H, E, N).
do_build_list([], E, _) ->
  E;
do_build_list([T|H], R, N) ->
  E = {cons, N, build_value(T, N), R},
  do_build_list(H, E, N).

build_tuple(T, N) when is_tuple(T) ->
  TupleContent = lists:map(fun(V) -> build_value(V, N) end, tuple_to_list(T)),
  {tuple, N, TupleContent}.

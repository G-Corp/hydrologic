-module(hydrologic_compiler).
-include("../include/hydrologic_utils.hrl").

-export([compile/2]).

compile(Name, Options) ->
  Mod = erl_syntax:atom(bucs:to_atom(Name)),

  case compile:forms([module(Mod)
                      , export()
                      , run_function(Mod)
                      % TODO: , new_function()
                     ]) of
    Compiled when element(1, Compiled) =:= ok ->
      [ok, _, Bin|Info] = tuple_to_list(Compiled),
      case Info of
        [Warnings] when length(Warnings) > 0 ->
          ?WARNING({Name, Warnings}, Options);
        _ ->
          ok
      end,
      lists:foldl(fun(Fun, Options0) ->
                      Fun(Name, Bin, Options0)
                  end,
                  Options,
                  [fun write_beam/3,
                   fun load_beam/3]);
    error ->
      ?ERROR({compilation_failed, Name}, Options),
      Options;
    {error, Errors, Warnings} ->
      ?ERROR({Name, Errors}, Options),
      ?WARNING({Name, Warnings}, Options),
      Options
  end.

write_beam(_, _, #{out_dir := false} = Options) ->
  Options;
write_beam(Name, Bin, #{out_dir := OutDir} = Options) ->
  BeamFile = filename:join([OutDir, [bucs:to_string(Name), ".beam"]]),
  case file:write_file(BeamFile, Bin) of
    ok ->
      Options;
    {error, Error} ->
      ?ERROR({Name, [Error]}, Options),
      Options
  end;
write_beam(Name, _, Options) ->
  ?WARNING({Name, missing_out_dir}, Options),
  Options.

load_beam(Name, Bin, #{no_load := false} = Options) ->
  code:purge(Name),
  case code:load_binary(Name, atom_to_list(Name) ++ ".erl", Bin) of
    {module, Name} ->
      Options;
    Error ->
      ?ERROR({Name, Error}, Options),
      Options
  end;
load_beam(_, _, Options) ->
  Options.

module(Mod) ->
  Module = erl_syntax:attribute(
             erl_syntax:atom(module),
             [Mod]),
  erl_syntax:revert(Module).

export() ->
  Export = erl_syntax:attribute(
             erl_syntax:atom(export),
             [erl_syntax:list(
                [erl_syntax:arity_qualifier(
                   erl_syntax:atom(run),
                   erl_syntax:integer(1))])]),
  erl_syntax:revert(Export).

run_function(Mod) ->
  % run(Data) -> hydrologic:run(Name, Data).
  Var = erl_syntax:variable("Data"),
  Body = erl_syntax:application(
           erl_syntax:atom(hydrologic),
           erl_syntax:atom(run),
           [Mod, Var]),
  Clause =  erl_syntax:clause([Var], [], [Body]),
  Function =  erl_syntax:function(erl_syntax:atom(run), [Clause]),
  erl_syntax:revert(Function).


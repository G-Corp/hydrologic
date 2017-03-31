-module(hydrologic_compiler).
-include("../include/hydrologic_utils.hrl").

% options :
%
% * compile_options => [atom()]
% * out_dir => string() | false
% * no_load => true | false

-export([compile/2]).

compile(Name, #{compile_options := CompileOptions} = Options) ->
  Mod = erl_syntax:atom(bucs:to_atom(Name)),

  Module = [module(Mod)
   , export([{run, 1}, {flow, 1}, {stop, 0}])
   , exec_functions(run, Mod)
   , exec_functions(flow, Mod)
   , stop_function(Mod)
   % TODO: , new_function()
  ],
  Formatted = erl_prettypr:format(erl_syntax:form_list(Module)),
  io:format("-----~n~s~n-----~n", [Formatted]),

  case compile:forms(Module, CompileOptions) of
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
  end;
compile(Name, Options) ->
  compile(Name, Options#{compile_options => [verbose, report_errors, report_warnings]}).

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

load_beam(_, _, #{no_load := true} = Options) ->
  Options;
load_beam(Name, Bin, #{no_load := false} = Options) ->
  ?INFO({load, Name}, Options),
  code:purge(Name),
  case code:load_binary(Name, atom_to_list(Name) ++ ".erl", Bin) of
    {module, Name} ->
      Options;
    Error ->
      ?ERROR({Name, Error}, Options),
      Options
  end;
load_beam(Name, _, Options) ->
  ?WARNING({Name, missing_no_load}, Options),
  Options.

module(Mod) ->
  Module = erl_syntax:attribute(
             erl_syntax:atom(module),
             [Mod]),
  erl_syntax:revert(Module).

export(Functions) ->
  Export = erl_syntax:attribute(
             erl_syntax:atom(export),
             [erl_syntax:list(
                [erl_syntax:arity_qualifier(
                   erl_syntax:atom(Name),
                   erl_syntax:integer(Arity)) || {Name, Arity} <- Functions])]),
  erl_syntax:revert(Export).

exec_functions(Name, Mod) ->
  % run(Data) -> hydrologic:Name(Mod, Data).
  Var = erl_syntax:variable("Data"),
  Body = erl_syntax:application(
           erl_syntax:atom(hydrologic),
           erl_syntax:atom(Name),
           [Mod, Var]),
  Clause =  erl_syntax:clause([Var], [], [Body]),
  Function =  erl_syntax:function(erl_syntax:atom(Name), [Clause]),
  erl_syntax:revert(Function).

stop_function(Mod) ->
  Body = erl_syntax:application(
           erl_syntax:atom(hydrologic),
           erl_syntax:atom(stop),
           [Mod]),
  Clause =  erl_syntax:clause([], [], [Body]),
  Function =  erl_syntax:function(erl_syntax:atom(stop), [Clause]),
  erl_syntax:revert(Function).


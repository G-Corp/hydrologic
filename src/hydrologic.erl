-module(hydrologic).
-include("../include/hydrologic.hrl").

-export([
         new/2,
         run/2,
         flow/2,
         stop/1
        ]).

-export_type([
              reduct/0
             ]).

-type operation() :: term().
-type pipe() :: atom().
-type data() :: any().
-type reduct() :: data() | {data(), any()} | {'__end__', any()}.

% @doc
% Create a new pipe.
% @end
-spec new(atom(), [operation()]) -> pipe().
new(Name, Operations) when is_list(Operations) ->
  create(Name, Operations, self()).

% @doc
% Execute a pipe treatment.
% @end
-spec run(pipe(), data()) -> {ok, data()}
                             | {error, term(), data()}
                             | {error, term()}
                             | any().
run(Pipe, Data) ->
  case ets:lookup(Pipe, config) of
    [{config, #{start := StartPID}}] when ?IS_RECEIVER(StartPID) ->
      StartPID ! #{
        name => Pipe,
        uuid => uuid:to_string(uuid:uuid4()),
        fan => -1,
        flow => ?FLOW(Data),
        data => Data,
        error => none},
      receive
        #{error := none, data := Response, flow := false} ->
          {ok, Response};
        #{error := none, data := Response, flow := true} ->
          {ok, remove_empty(Response)};
        #{error := Error, data := Response} ->
          {error, Error, Response};
        Other ->
          Other
      end;
    _ ->
      {error, invalid_pipe}
  end.

% @equiv run(Pipe, Data)
flow(Pipe, Data) ->
  run(Pipe, Data).

% @doc
% Destroy the given pipe.
% @end
-spec stop(pipe()) -> ok | {error, term()}.
stop(Pipe) ->
  case ets:lookup(Pipe, config) of
    [{config, #{processes := ProcessPIDs,
                name := Name}}] ->
      [begin
         case is_pid(PID) of
           true ->
             erlang:exit(PID, normal);
           false ->
             erlang:exit(erlang:whereis(PID), normal),
             erlang:unregister(PID)
         end
       end || PID <- ProcessPIDs],
      ets:delete(Name),
      ok;
    _ ->
      {error, invalid_pipe}
  end.

create(Name, Operations, PID) ->
  {StartPID, ProcessPIDs} = lists:foldr(fun(Worker, {Receiver, PIDs}) ->
                                            WPID = make_worker(Worker, Receiver, PID),
                                            {WPID, [WPID|PIDs]}
                                        end, {PID, []}, Operations),
  ets:new(Name, [named_table, public]),
  ets:insert(Name, {config, #{name => Name,
                              final => PID,
                              start => StartPID,
                              processes => ProcessPIDs}}),
  Name.

make_worker({Label, Worker}, Receiver, EndPID) when ?IS_RECEIVER(Label),
                                                    ?IS_WORKER(Worker),
                                                    ?IS_RECEIVER(Receiver),
                                                    is_pid(EndPID) ->
  WorkerPID = make_worker(Worker, Receiver, EndPID),
  erlang:register(Label, WorkerPID),
  Label;
make_worker(Worker, Receiver, EndPID) when ?IS_WORKER(Worker),
                                           ?IS_RECEIVER(Receiver),
                                           is_pid(EndPID) ->
  erlang:spawn_link(fun() ->
                        worker(Worker, Receiver, EndPID)
                    end).

worker({merge, Function}, PID, EndPID) when ?IS_RECEIVER(PID),
                                            ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      PID ! eof;
    #{fan := -1} = Record ->
      PID ! Record,
      worker(merge, PID, EndPID);
    Record ->
      case merge(Record, PID, Function) of
        ok ->
          worker({merge, Function}, PID, EndPID);
        error ->
          PID ! eof % TODO: stop the stream
      end
  end;
worker(fanin, PID, EndPID) when ?IS_RECEIVER(PID),
                                ?IS_RECEIVER(EndPID) ->
  worker({fanin, 0}, PID, EndPID);
worker({fanin, 0}, PID, EndPID) ->
  worker({merge, fun(X, _) -> X end}, PID, EndPID);
worker({fanin, 1}, PID, EndPID) ->
  worker({merge, fun(_, X) -> X end}, PID, EndPID);
worker({duplicate, AltPID}, PID, EndPID) when ?IS_RECEIVER(AltPID),
                                              ?IS_RECEIVER(PID),
                                              ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      PID ! eof;
    Record ->
      AltPID ! Record#{fan => 1},
      PID ! Record#{fan => 0},
      worker({duplicate, AltPID}, PID, EndPID)
  end;
worker(Fun, PID, EndPID) when is_function(Fun, 1),
                              ?IS_RECEIVER(PID),
                              ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      PID ! eof;
    #{flow := false} = Record ->
      response(PID, EndPID, Record, Fun),
      worker(Fun, PID, EndPID);
    #{flow := true} = Record ->
      flow_response(PID, EndPID, Record, Fun),
      worker(Fun, PID, EndPID)
  end;
worker({Fun, AltPID} = Worker, PID, EndPID) when is_function(Fun, 1),
                                                 ?IS_RECEIVER(AltPID),
                                                 ?IS_RECEIVER(PID),
                                                 ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      PID ! eof;
    #{flow := false} = Record ->
      response(PID, AltPID, EndPID, Record, Fun),
      worker(Worker, PID, EndPID);
    #{flow := true} = Record ->
      flow_response(PID, AltPID, EndPID, Record, Fun),
      worker(Worker, PID, EndPID)
  end;
worker({Module, Function, Args} = Worker, PID, EndPID) when is_atom(Module),
                                                            is_atom(Function),
                                                            is_list(Args),
                                                            ?IS_RECEIVER(PID),
                                                            ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      PID ! eof;
    #{flow := false} = Record ->
      response( PID, EndPID, Record, {Module, Function, Args}),
      worker(Worker, PID, EndPID);
    #{flow := true} = Record ->
      flow_response(PID, EndPID, Record, {Module, Function, Args}),
      worker(Worker, PID, EndPID)
  end;
worker({{Module, Function, Args}, AltPID} = Worker, PID, EndPID) when is_atom(Module),
                                                                      is_atom(Function),
                                                                      is_list(Args),
                                                                      ?IS_RECEIVER(AltPID),
                                                                      ?IS_RECEIVER(PID),
                                                                      ?IS_RECEIVER(EndPID) ->
  receive
    eof ->
      PID ! eof;
    #{flow := false} = Record ->
      response(PID, AltPID, EndPID, Record, {Module, Function, Args}),
      worker(Worker, PID, EndPID);
    #{flow := true} = Record ->
      flow_response(PID, AltPID, EndPID, Record, {Module, Function, Args}),
      worker(Worker, PID, EndPID)
  end;
worker({ModuleOrFunction, FunctionOrLabel}, PID, EndPID) when is_atom(ModuleOrFunction),
                                                              is_atom(FunctionOrLabel),
                                                              ?IS_RECEIVER(PID),
                                                              ?IS_RECEIVER(EndPID) ->
  case bucs:function_exists(ModuleOrFunction, FunctionOrLabel, 1) of
    true ->
      worker({ModuleOrFunction, FunctionOrLabel, []}, PID, EndPID);
    false ->
      worker({{hydrologic_stdlib, ModuleOrFunction, []}, FunctionOrLabel}, PID, EndPID)
  end;
worker({{Module, Function}, AltPID}, PID, EndPID) when is_atom(Module),
                                                       is_atom(Function),
                                                       ?IS_RECEIVER(AltPID),
                                                       ?IS_RECEIVER(PID),
                                                       ?IS_RECEIVER(EndPID) ->
  worker({{Module, Function, []}, AltPID}, PID, EndPID);
worker(FunctionOrReceiver, PID, EndPID) when is_atom(FunctionOrReceiver),
                                             ?IS_RECEIVER(PID),
                                             ?IS_RECEIVER(EndPID) ->
  case bucs:function_exists(hydrologic_stdlib, FunctionOrReceiver, 1) of
    true ->
      worker({hydrologic_stdlib, FunctionOrReceiver, []}, PID, EndPID);
    false ->
      case whereis(FunctionOrReceiver) of
        undefined ->
          EndPID ! {error, {undefined, FunctionOrReceiver}};
        _ ->
          gosub(FunctionOrReceiver, PID),
          worker(FunctionOrReceiver, PID, EndPID)
      end
  end;
worker({Function, Args}, PID, EndPID) when is_atom(Function),
                                           is_list(Args) ,
                                           ?IS_RECEIVER(PID),
                                           ?IS_RECEIVER(EndPID) ->
  worker({hydrologic_stdlib, Function, Args}, PID, EndPID);
worker({Function, AltPID}, PID, EndPID) when is_atom(Function),
                                             ?IS_RECEIVER(AltPID),
                                             ?IS_RECEIVER(PID),
                                             ?IS_RECEIVER(EndPID) ->
  worker({{hydrologic_stdlib, Function, []}, AltPID}, PID, EndPID);
worker({{Function, Args}, AltPID}, PID, EndPID) when is_atom(Function),
                                                     is_list(Args),
                                                     ?IS_RECEIVER(AltPID),
                                                     ?IS_RECEIVER(PID),
                                                     ?IS_RECEIVER(EndPID) ->
  worker({{hydrologic_stdlib, Function, Args}, AltPID}, PID, EndPID).

response(PID, EndPID, #{data := Data} = Record, Function) ->
  case is_receiver(PID) of
    true ->
      case callfun2(Function, [Data]) of
        {map, NewData} ->
          PID ! Record#{data => NewData, flow => ?FLOW(NewData)};
        {filter, true} ->
          PID ! Record;
        {filter, false} ->
          EndPID ! ok;
        {return, NewData} ->
          EndPID ! Record#{data => NewData};
        {reduce, NewData} ->
          PID ! Record#{data => NewData, flow => ?FLOW(NewData)};
        {error, Error} ->
          EndPID ! Record#{error => Error};
        Other ->
          PID ! Record#{data => Other}
      end;
    false ->
      EndPID ! Record#{error => invalid_pipe}
  end.
response(PID, AltPID, EndPID, #{data := Data} = Record, Function) ->
  case is_receiver(PID) of
    true ->
      case callfun2(Function, [Data]) of
        {map, NewData} ->
          PID ! Record#{data => NewData, flow => ?FLOW(NewData)};
        {filter, true} ->
          PID ! Record;
        {filter, false} ->
          AltPID ! Record;
        {return, NewData} ->
          EndPID ! Record#{data => NewData};
        {reduce, NewData} ->
          PID ! Record#{data => NewData, flow => ?FLOW(NewData)};
        {error, Error} ->
          EndPID ! Record#{error => Error};
        Other ->
          PID ! Record#{data => Other}
      end;
    false ->
      EndPID ! Record#{error => invalid_pipe}
  end.

flow_response(PID, EndPID, #{data := Data} = Record, Function) ->
  case is_receiver(PID) of
    true ->
      case callfun5(Data, Function, '$', [], []) of
        {map, NewData, _} ->
          PID ! Record#{data => NewData};
        {filter, NewData, _} ->
          PID ! Record#{data => NewData};
        {reduce, NewData} ->
          PID ! Record#{data => NewData, flow => ?FLOW(NewData)};
        {return, NewData, _} ->
          EndPID ! Record#{data => NewData};
        {error, Error} ->
          EndPID ! Record#{error => Error};
        {'$', _, _} ->
          PID ! Record;
        {_, Data} ->
          PID ! Record
      end;
    false ->
      EndPID ! Record#{error => invalid_pipe}
  end.
flow_response(PID, AltPID, EndPID, #{data := Data} = Record, Function) ->
  case is_receiver(PID) of
    true ->
      case callfun5(Data, Function, '$', [], []) of
        {map, NewData, _} ->
          PID ! Record#{data => NewData};
        {filter, NewData0, NewData1} ->
          PID ! Record#{data => NewData0, fan => 0},
          AltPID ! Record#{data => NewData1, fan => 1};
        {reduce, NewData} ->
          PID ! Record#{data => NewData, flow => ?FLOW(NewData)};
        {return, NewData, _} ->
          EndPID ! Record#{data => NewData};
        {error, Error} ->
          EndPID ! Record#{error => Error};
        {'$', _, _} ->
          PID ! Record;
        {_, Data} ->
          PID ! Record
      end;
    false ->
      EndPID ! Record#{error => invalid_pipe}
  end.

gosub(Receiver, PID) ->
  receive
    eof ->
      PID ! eof;
    Data ->
      Receiver ! Data,
      gosub(Receiver, PID)
  end.

merge(#{fan := Fan1, uuid := UUID, data := Data1, name := Name, flow := Flow} = Record, PID, Function) ->
  case ets:lookup(Name, UUID) of
    [{UUID, #{fan := Fan0, uuid := UUID, data := Data0, name := Name, flow := Flow}}] ->
      ets:delete(Name, UUID),
      NewData = case Flow of
                  false -> callfun2(Function,
                                    case Fan0 < Fan1 of
                                      true ->
                                        [Data0, Data1];
                                      false ->
                                        [Data1, Data0]
                                    end);
                  true ->
                    merge_flow(case Fan0 < Fan1 of
                                 true ->
                                   {Data0, Data1};
                                 false ->
                                   {Data1, Data0}
                               end, Function)
                end,
      PID ! #{name => Name,
              uuid => UUID,
              fan => -1,
              flow => Flow,
              data => NewData,
              error => none},
      ok;
    [] ->
      ets:insert(Name, {UUID, Record}),
      ok;
    _ ->
      error
  end.

callfun2({Module, Function, Args0}, Args1) when is_atom(Module),
                                                is_atom(Function),
                                                is_list(Args0),
                                                is_list(Args1) ->
  callfun2({Module, Function}, Args1 ++ Args0);
callfun2({Module, Function}, Args) when is_atom(Module),
                                        is_atom(Function),
                                        is_list(Args) ->
  erlang:apply(Module, Function, Args);
callfun2({Function, Args0}, Args1) when is_list(Args0) andalso
                                        is_list(Args1) andalso
                                        is_function(Function, length(Args0) + length(Args1)) ->
  callfun2(Function, Args1 ++ Args0);
callfun2(Function, Args) when is_list(Args) andalso
                              is_function(Function, length(Args)) ->
  erlang:apply(Function, Args).

callfun5([], _, reduce, Reduce, []) ->
  Reduce;
callfun5([], _, Type, Acc0, Acc1) ->
  {Type, lists:reverse(Acc0), lists:reverse(Acc1)};
callfun5(['$empty$'|Rest], Function, Type, Acc0, Acc1) ->
  callfun5(Rest, Function, Type, ['$empty$'|Acc0], Acc1);
callfun5([Data|Rest], Function, Type, Acc0, Acc1) ->
  next_call(Data, Rest, Function, Type, Acc0, Acc1, callfun2(Function, [Data])).

next_call(_, Rest, Function, Type0, Acc0, Acc1, {map, NewData}) when Type0 == map;
                                                                     Type0 == '$' ->
  callfun5(Rest, Function, map, [NewData|Acc0], Acc1);
next_call(Data, Rest, Function, Type0, Acc0, Acc1, {filter, true}) when Type0 == filter;
                                                                        Type0 == '$' ->
  callfun5(Rest, Function, filter, [Data|Acc0], ['$empty$'|Acc1]);
next_call(Data, Rest, Function, Type0, Acc0, Acc1, {filter, false}) when Type0 == filter;
                                                                         Type0 == '$' ->
  callfun5(Rest, Function, filter, ['$empty$'|Acc0], [Data|Acc1]);
next_call(_, Rest, Function, Type0, Acc0, Acc1, {return, NewData}) when Type0 == return;
                                                                        Type0 == '$' ->
  callfun5(Rest, Function, return, [NewData|Acc0], Acc1);
next_call(_, Rest, Function, '$', _, [], Reduce = {reduce, _}) ->
  callfun5([], Function, reduce, reduce(Rest, Function, Reduce), []);
next_call(_, _, _, _, _, _, {error, _} = Error) ->
  Error;
next_call(_, Rest, Function, Type0, Acc0, Acc1, Other) when Type0 == map;
                                                            Type0 == '$' ->
  callfun5(Rest, Function, map, [Other|Acc0], Acc1).

reduce([], Function, {reduce, Acc}) ->
  callfun2(Function, [{'__end__', Acc}]);
reduce([Data|Rest], Function, {reduce, Acc}) ->
  reduce(Rest, Function, callfun2(Function, [{Data, Acc}])).

remove_empty([]) ->
  [];
remove_empty(['$empty$'|Rest]) ->
  remove_empty(Rest);
remove_empty([X|Rest]) ->
  [X|remove_empty(Rest)].

merge_flow({[], []}, _) ->
  [];
merge_flow({[], Rest1}, _) ->
  Rest1;
merge_flow({Rest0, []}, _) ->
  Rest0;
merge_flow({[Data0|Rest0], ['$empty$'|Rest1]}, Function) ->
  [Data0|merge_flow({Rest0, Rest1}, Function)];
merge_flow({['$empty$'|Rest0], ['$empty$'|Rest1]}, Function) ->
  ['$empty$'|merge_flow({Rest0, Rest1}, Function)];
merge_flow({['$empty$'|Rest0], [Data1|Rest1]}, Function) ->
  [Data1|merge_flow({Rest0, Rest1}, Function)];
merge_flow({[Data0|Rest0], ['$empty$'|Rest1]}, Function) ->
  [Data0|merge_flow({Rest0, Rest1}, Function)];
merge_flow({[Data0|Rest0], [Data1|Rest1]}, Function) ->
  [erlang:apply(Function, [Data0, Data1])|merge_flow({Rest0, Rest1}, Function)].

is_receiver(Anchor) when is_pid(Anchor) -> true;
is_receiver(Anchor) when is_atom(Anchor) ->
  is_pid(erlang:whereis(Anchor));
is_receiver(_) -> false.


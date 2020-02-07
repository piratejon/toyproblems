-module(solve).
-export([part1/0, part2/0]).

read_input_file(Filename) ->
  {ok, Data} = file:read_file(Filename),
  binary:split(Data, [<<",">>], [global]).

fetch_input(Filename) ->
  InputLines = read_input_file(Filename),
  CleansedInputLines = lists:map(fun(L) -> lists:takewhile(fun(C) -> ((C >= $0) and (C =< $9)) end, binary_to_list(L)) end, InputLines),
  lists:map(fun list_to_integer/1, CleansedInputLines).

chunk_program(List, N) when length(List) > N ->
  [lists:sublist(List, N) | chunk_program(lists:nthtail(N, List), N)];
chunk_program(List, _) -> [List].

part1_by_index(Pos, {_, Program}) ->
  [CheckOp | _] = lists:nthtail(Pos, Program),
  %io:fwrite("part1_by_index ~w~n", [[Pos, Program]]),
  case CheckOp of
    99 -> {break, Program};
    _ ->
      [Op, Ai, Bi, Out | _] = lists:nthtail(Pos, Program),
      %io:fwrite("part1_by_index ~w~n", [[Op, Ai, Bi, Out]]),
      A = lists:nth(Ai + 1, Program),
      B = lists:nth(Bi + 1, Program),
      case Op of
        1 -> {ok, impute_at(Program, Out, A + B)};
        2 -> {ok, impute_at(Program, Out, A * B)}
      end
  end;
part1_by_index(Pos, Program) -> part1_by_index(Pos, {ok, Program}).

impute_at(Program, Position, Value) ->
  lists:sublist(Program, Position) ++ [Value] ++ lists:nthtail(Position + 1, Program).

% <http://erlang.org/pipermail/erlang-questions/2009-June/044173.html>
% 2019-12-03
bfoldl(Fun, Acc0, [Head | Tail]) ->
  case Fun(Head, Acc0) of
    {ok, Acc} -> bfoldl(Fun, Acc, Tail);
    {break, _Acc} = Result -> Result;
    break -> {break, Acc0}
  end;
bfoldl(_, Acc0, []) -> {ok, Acc0}.

part1() ->
  Program = fetch_input('input'),
  Patched = impute_at(impute_at(Program, 1, 12), 2, 2),
  io:fwrite("input ~w~n", [Patched]),
  {_, [Answer | _]} = bfoldl(fun part1_by_index/2, Patched, lists:seq(0, length(Patched) - 1, 4)),
  io:fwrite("part1 ~w~n", [Answer]).

part2_inner(Program, Noun, Verb, ProgramWalker) ->
  Patched = impute_at(impute_at(Program, 1, Noun), 2, Verb),
  {_, [Output | _]} = bfoldl(fun part1_by_index/2, Patched, ProgramWalker),
  Output.

part2_what(Sought, Program, Walker, {Noun, Verb}, A) ->
  case part2_inner(Program, Noun, Verb, Walker) of
    Sought -> {break, {Noun, Verb}};
    _ -> {ok, {Noun, Verb}}
  end.

part2() ->
  Program = fetch_input('input'),
  Range = lists:seq(0, 99),
  Sought = 19690720,
  Product = [{A, B} || A <- Range, B <- Range],
  Walker = lists:seq(0, length(Program) - 1, 4),
  {_, {Noun, Verb}} = bfoldl(fun(E,A) -> part2_what(Sought, Program, Walker, E, A) end, {ok, {-1, -1}}, Product),
  io:fwrite("part2 ~w~n", [(100 * Noun) + Verb]).

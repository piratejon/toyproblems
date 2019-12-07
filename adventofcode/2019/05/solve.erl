-module(solve).
-export([part1/0, part2/0]).

-record(state, {program, ip, input, output}).

read_input_file(Filename) ->
  {ok, Data} = file:read_file(Filename),
  binary:split(Data, [<<",">>], [global]).

fetch_input(Filename) ->
  InputLines = read_input_file(Filename),
  CleansedInputLines = [lists:takewhile(fun(C) -> (C >= $-) and (C =< $9) end, binary_to_list(L)) || L <- InputLines],
  [list_to_integer(A) || A <- CleansedInputLines].

impute_at(Program, Position, Value) ->
  lists:sublist(Program, Position) ++ [Value] ++ lists:nthtail(Position + 1, Program).

program_read(Program, Position) ->
  lists:nth(Position + 1, Program).

program_read_tail(Program, Position) ->
  lists:nthtail(Position, Program).

program_write(Program, Position, Value) ->
  impute_at(Program, Position, Value).

% <http://erlang.org/pipermail/erlang-questions/2009-June/044173.html>
% 2019-12-03
bfoldl(Fun, Acc0, [Head | Tail]) ->
  case Fun(Head, Acc0) of
    {ok, Acc} -> bfoldl(Fun, Acc, Tail);
    {break, _Acc} = Result -> Result;
    break -> {break, Acc0}
  end;
bfoldl(_, Acc0, []) -> {ok, Acc0}.

init_program(Input) ->
  #state{program=Input, ip=0, input=[1], output=[]}.

binop(1) -> fun(A, B) -> A + B end;
binop(2) -> fun(A, B) -> A * B end.

execute_binop(P, Opcode) ->
  [Opcode, Ai, Bi, Oi | _] = program_read_tail(P#state.program, P#state.ip),
  Read = [Opcode, Ai, Bi, Oi],
  A = program_read(P#state.program, Ai),
  B = program_read(P#state.program, Bi),
  io:fwrite("execute_binop Read: ~w~n", [[Read | [A, B]]]),
  P#state{ip=P#state.ip + 4, program=program_write(P#state.program, Oi, (binop(Opcode))(A, B))}.

execute_input(P, Opcode) ->
  [Opcode, Loc | _] = program_read_tail(P#state.program, P#state.ip),
  [ReadInput | RestInput] = P#state.input,
  P#state{
    ip=P#state.ip + 2
    , input=RestInput
    , program=program_write(P#state.program, Loc, ReadInput)
  }.

execute_output(P, Opcode) ->
  [Opcode, Loc | _] = program_read_tail(P#state.program, P#state.ip),
  P#state{
    ip=P#state.ip + 2
    , output=[program_read(P#state.program, Loc)] ++ P#state.output
  }.

execute(P) ->
  case lists:reverse(integer_to_list(Op = program_read(P#state.program, P#state.ip))) of
    [$1 | _] -> {continue, execute_binop(P, Op)};
    [$2 | _] -> {continue, execute_binop(P, Op)};
    [$3 | _] -> {continue, execute_input(P, Op)};
    [$4 | _] -> {continue, execute_output(P, Op)};
    [$9, $9 | _] -> {halt, P}
  end.

execute_loop(Program) ->
  case {_, NewProgram} = execute(Program) of
    {continue, _} -> execute_loop(NewProgram);
    {halt, _} -> Program
  end.

part1() ->
  Input = fetch_input('02-input'),
  Program = init_program(Input),
  FinalState = execute_loop(Program),
  io:fwrite("Output ~w~n", [FinalState#state.output]),
  %io:fwrite("Program ~w~n", [FinalState]),
  [].

part2() -> [].

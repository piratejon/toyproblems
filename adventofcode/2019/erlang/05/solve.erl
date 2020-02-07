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

unpack_opcode(Opcode) ->
  Size = 2 + case lists:reverse(List = integer_to_list(Opcode)) of
           [$1 | _] -> 3;
           [$2 | _] -> 3;
           [$3 | _] -> 1;
           [$4 | _] -> 1;
           [$9, $9 | _] -> 0
         end,
  %io:fwrite("unpack_opcode: ~w~n", [[Opcode, Size]]),
  %lists:reverse(lists:nthtail(Size - 1, [0 || _ <- lists:seq(1, Size)] ++ [list_to_integer([X]) || X <- List])).
  lists:sublist(lists:reverse([0 || _ <- lists:seq(1, Size)] ++ [list_to_integer([X]) || X <- List]), Size).

resolve_reference(P, Ref, Mode) ->
  R = case Mode of
    0 -> program_read(P#state.program, Ref);
    1 -> Ref
  end,
  %io:fwrite("resolve_reference: ~w~n", [[Ref, Mode, R]]),
  R.

resolve_references(P, Refs, Modes) ->
  Resolved = [resolve_reference(P, Ref, Mode) || {Ref, Mode} <- lists:zip(Refs, Modes)],
  io:fwrite("resolve_references: ~w~n", [[Refs, Modes, Resolved]]),
  Resolved.

execute_binop(P, Opcode) ->
  [Ar, Br, Cr | _] = program_read_tail(P#state.program, P#state.ip + 1),
  % don't resolve C reference because it's always a position
  [A, B, _] = resolve_references(P, [Ar, Br, Cr], lists:nthtail(2, Opcode)),
  io:fwrite("execute_binop: ~w~n", [[Opcode, A, B, Cr]]),
  {continue, P#state{ip=P#state.ip + 4, program=program_write(P#state.program, Cr, (binop(lists:nth(1, Opcode)))(A, B))}}.

execute_input(P, Unpacked) ->
  io:fwrite("execute_input: ~w~n", [Unpacked]),
  [Loc | _] = program_read_tail(P#state.program, P#state.ip + 1),
  io:fwrite("execute_input: ~w~n", [[Unpacked, Loc]]),
  [ReadInput | RestInput] = P#state.input,
  {continue, P#state{
    ip=P#state.ip + 2
    , input=RestInput
    , program=program_write(P#state.program, Loc, ReadInput)
  }}.

execute_output(P, Opcode) ->
  [Loc | _] = program_read_tail(P#state.program, P#state.ip + 1),
  io:fwrite("execute_output: ~w~n", [[Opcode, Loc]]),
  {continue, P#state{
    ip=P#state.ip + 2
    , output=[program_read(P#state.program, Loc)] ++ P#state.output
  }}.

execute_halt(P, _) -> {halt, P}.

execute(P) ->
  io:fwrite("dispatching ~w~n", [[P#state.ip, lists:sublist(program_read_tail(P#state.program, P#state.ip), 4)]]),
  Fun = case Opcode = unpack_opcode(program_read(P#state.program, P#state.ip)) of
    [1 | _] -> fun execute_binop/2;
    [2 | _] -> fun execute_binop/2;
    [3 | _] -> fun execute_input/2;
    [4 | _] -> fun execute_output/2;
    [9, 9 | _] -> fun execute_halt/2
  end,
  io:fwrite("dispatching ~w~n", [Opcode]),
  Fun(P, Opcode).

execute_loop(Program) ->
  case {_, NewProgram} = execute(Program) of
    {continue, _} -> execute_loop(NewProgram);
    {halt, _} -> Program
  end.

part1() ->
  Input = fetch_input('input'),
  Program = init_program(Input),
  FinalState = execute_loop(Program),
  io:fwrite("Output ~w~n", [FinalState#state.output]),
  %io:fwrite("Program ~w~n", [FinalState]),
  [].

part2() -> [].

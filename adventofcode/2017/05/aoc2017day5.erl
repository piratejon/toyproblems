-module(aoc2017day5).
-export([main/1]).

readlines(File) ->
  case io:fread(File, "", "~d") of
    %{ok, Data} -> [string:to_integer(lists:nth(1, Data)) | readlines(File)];
    {ok, Data} -> [lists:nth(1, Data) | readlines(File)];
    eof -> []
  end.

read(Filename) ->
  {ok, Device} = file:open(Filename, [read]),
  Lines = readlines(Device),
  file:close(Device),
  Lines.

solve_part2(Input, PC, UpperBound, Counter) ->
  if
    PC < 0 orelse PC >= UpperBound -> Counter;
    true ->
      JmpRel = array:get(PC, Input),
      Modify = if
          JmpRel >= 3 -> JmpRel - 1;
          true -> JmpRel + 1
      end,
      solve_part2(array:set(PC, Modify, Input), PC + JmpRel, UpperBound, Counter + 1)
  end.

solve_part2(Input, PC, UpperBound) ->
  solve_part2(Input, PC, UpperBound, 0).

solve_part1(Input, PC, UpperBound, Counter) ->
  if
    PC < 0 orelse PC >= UpperBound -> Counter;
    true ->
      JmpRel = array:get(PC, Input),
      solve_part1(array:set(PC, JmpRel + 1, Input), PC + JmpRel, UpperBound, Counter + 1)
  end.

solve_part1(Input, PC, UpperBound) ->
  solve_part1(Input, PC, UpperBound, 0).

part1(Filename) ->
  List = read(Filename),
  Input = array:from_list(List),
  io:format("Input length: ~w~n", [array:size(Input)]),
  %io:format("Array: ~w~n", [Input]),
  io:format("Part 1 result: ~w~n", [solve_part1(Input, 0, array:size(Input))]).

part2(Filename) ->
  List = read(Filename),
  Input = array:from_list(List),
  io:format("Input length: ~w~n", [array:size(Input)]),
  io:format("Part 2 result: ~w~n", [solve_part2(Input, 0, array:size(Input))]).

main(_) ->
  %part1('input'),
  part2('input'),
  true.

-module(solve).
-export([test/0, part1/0, part2/0]).

% <https://stackoverflow.com/a/16986266/5403184> 2020-12-05
read_input_lines(Filename) ->
  {ok,  Data} = file:read_file(Filename),
  binary:split(Data, [<<"\n">>], [global]).

fetch_input(Filename) ->
  [binary_to_list(L) || L <- read_input_lines(Filename), L /= <<>>].

solve_part1(Input, Over, Down) ->
  RowCount = length(Input),
  ColCount = length(lists:nth(1, Input)),
  RowOffsets = lists:seq(1, RowCount, Down),
  Rows = lists:filtermap(
           fun ({E, I}) -> case (I - 1) rem Down of 0 -> {true, E}; _ -> false end end,
           lists:zip(Input, lists:seq(1, length(Input)))),
  ColOffsets = [(((R - 1) * Over) rem ColCount) + 1 || R <- lists:seq(1, length(RowOffsets))],
  io:fwrite("RowCount: ~w, ColCount: ~w~nRO: ~w~nCO: ~w~n", [RowCount, ColCount, RowOffsets, ColOffsets]),
  Trees = lists:filter(
    fun ({Row, Offset}) -> lists:nth(Offset, Row) == 35 end,
    lists:nthtail(1, lists:zip(Rows, ColOffsets))),
  io:fwrite("trees: ~w~n", [length(Trees)]),
  length(Trees).

solve_part2(Input) ->
  solve_part1(Input, 1, 1) *
  solve_part1(Input, 3, 1) *
  solve_part1(Input, 5, 1) *
  solve_part1(Input, 7, 1) *
  solve_part1(Input, 1, 2).

test() ->
  Input = fetch_input('test.txt'),
  Sol1 = solve_part1(Input, 3, 1),
  io:fwrite("Test Part1 ~w~n", [Sol1]),
  Sol2 = solve_part2(Input),
  io:fwrite("Test Part2 ~w~n", [Sol2]),
  [].

part1() ->
  Input = fetch_input('input.txt'),
  Sol1 = solve_part1(Input, 3, 1),
  io:fwrite("Part1 ~w~n", [Sol1]),
  [].

part2() ->
  Input = fetch_input('input.txt'),
  Sol2 = solve_part2(Input),
  io:fwrite("Part2 ~w~n", [Sol2]),
  [].

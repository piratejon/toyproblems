-module(solve).
-export([test/0, part1/0, part2/0]).

% <https://stackoverflow.com/a/16986266/5403184> 2020-12-05
read_input_lines(Filename) ->
  {ok,  Data} = file:read_file(Filename),
  Lines = binary:split(Data, [<<"\n">>], [global]),
  case lists:last(Lines) of
    <<>> -> lists:droplast(Lines);
    _ -> Lines
  end.

fetch_input(Filename) ->
  [fetch_int(binary_to_list(Line)) || Line <- read_input_lines(Filename)].

fetch_int(String) ->
  case string:to_integer(String) of
    {A, []} -> A;
    _ -> wtf
  end.

make_pairs(List) ->
  TaggedList = lists:zip(List, lists:seq(1, length(List))),
  [{A, B} || {A, At} <- TaggedList, {B, Bt} <- TaggedList, At < Bt].

check_value(Preamble, Value) ->
  Pairs = make_pairs(Preamble),
  lists:any(fun ({A, B}) -> (A + B) == Value end, Pairs).

solve_part1(Input, PreambleSize) ->
  Windows = [ lists:split(PreambleSize, lists:sublist(Input, I, PreambleSize + 1))
              || I <- lists:seq(1, length(Input) - PreambleSize)],
  lists:nth(1, lists:dropwhile(fun ({P, [V]}) -> check_value(P, V) end, Windows)).

solve_part2(Input, Part1) ->
  {_, [Bad]} = Part1,
  Indices = lists:seq(1, length(Input)),
  [{_, Range}] = [ {{At, Bt}, lists:sublist(Input, At, (Bt - At) + 1)}
                || At <- Indices,
                   Bt <- Indices,
                   At < Bt,
                   Bad == lists:sum(lists:sublist(Input, At, (Bt - At) + 1))
              ],
  {Range, lists:min(Range) + lists:max(Range)}.

test() ->
  TestInput = fetch_input('test.txt'),
  Part1 = solve_part1(TestInput, 5),
  io:fwrite("Test Input: ~p~n", [Part1]),
  Part2 = solve_part2(TestInput, Part1),
  io:fwrite("Part 2: ~p~n", [Part2]),
  [].

part1() ->
  Input = fetch_input('input.txt'),
  Part1 = solve_part1(Input, 25),
  io:fwrite("Result: ~p~n", [Part1]),
  Part2 = solve_part2(Input, Part1),
  io:fwrite("Part 2: ~p~n", [Part2]),
  [].

part2() ->
  [].

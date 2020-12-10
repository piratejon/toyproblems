-module(solve).
-export([test/0, part1/0, part2/0]).

get_parser() ->
  case re:compile("([\\d]+)-([\\d]+) ([a-z]): ([a-z]+)") of
    {ok, MP} -> MP % YOLO
  end.

% <https://stackoverflow.com/a/16986266/5403184> 2020-12-05
read_input_lines(Filename) ->
  {ok,  Data} = file:read_file(Filename),
  binary:split(Data, [<<"\n">>], [global]).

fetch_input(Filename) ->
  [binary_to_list(L) || L <- read_input_lines(Filename), L /= <<>>].

fetch_int(String) ->
  case string:to_integer(String) of
    {A, <<>>} -> A % YOLO
  end.

rotate_left(Count, List) ->
  Count2 = Count rem length(List),
  if
    Count2 > 0 -> lists:nthtail(Count2, List) ++ lists:sublist(List, Count2);
    Count2 == 0 -> List
  end.

solve_part1(Input, Over, Down) ->
  Rows = length(Input),
  StartInput = lists:nthtail(1, Input),
  Cols = length(lists:nth(1, Input)),
  Shifts = lists:seq(Over, ((Rows - 1) * (Cols div Over)), Over),
  Shifted = [rotate_left(S, L) || {L, S} <- lists:zip(StartInput, Shifts)],
  %[io:fwrite("~w~n", [list_to_binary(L)]) || L <- Shifted].
  length(lists:filter(fun (L) -> lists:nth(1, L) == 35 end, Shifted)).

test() ->
  Input = fetch_input('test.txt'),
  io:fwrite("input ~w~n", [Input]),
  io:fwrite("first ~w~n", [lists:nth(1, Input)]),
  Sol1 = solve_part1(Input, 3, 1),
  io:fwrite("size ~w~n", [Sol1]),
  %ValidPart1 = lists:filtermap(fun is_valid_part1/1, Input),
  %io:fwrite("part 1 ~w~n", [length(ValidPart2)]),
  [].

part1() ->
  Input = fetch_input('input.txt'),
  Sol1 = solve_part1(Input, 3, 1),
  io:fwrite("size ~w~n", [Sol1]),
  [].

part2() ->
  %Input = fetch_input('input.txt'),
  %Valids = lists:filtermap(fun is_valid_part2/1, Input),
  %io:fwrite("test input ~w~n", [Input]),
  %io:fwrite("part2      ~w~n", [length(Valids)]),
  [].

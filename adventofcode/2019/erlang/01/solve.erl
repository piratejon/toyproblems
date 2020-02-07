-module(solve).
-export([part1/0, part2/0]).

read_lines(File) ->
  case io:fread(File, "", "~d") of
    {ok, Data} -> [lists:nth(1, Data) | read_lines(File)];
    eof -> []
  end.

read_input_file(Filename) ->
  {ok, Device} = file:open(Filename, [read]),
  Lines = read_lines(Device),
  file:close(Device),
  Lines.

calculate_fuel(Weight) ->
  (Weight div 3) - 2.

part1() ->
  Weights = read_input_file('input'),
  FuelWeights = lists:map(fun calculate_fuel/1, Weights),
  % io:fwrite("weights: ~w~n", [Weights]),
  % io:fwrite("fuel weights: ~w~n", [FuelWeights]),
  io:fwrite("part1 ~w~n", [lists:sum(FuelWeights)]).

part2_item_loop(Weight) ->
  ItemFuel = calculate_fuel(Weight),
  if
    ItemFuel > 0 -> [ItemFuel | part2_item_loop(ItemFuel)];
    true -> []
  end.

part2_each_item(Weight) ->
  lists:sum(part2_item_loop(Weight)).

part2() ->
  Weights = read_input_file('input'),
  ItemWeights = lists:map(fun part2_each_item/1, Weights),
  io:fwrite("part2 ~w~n", [lists:sum(ItemWeights)]).

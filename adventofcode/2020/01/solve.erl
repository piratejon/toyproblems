-module(solve).
-export([test/0, part1/0, part2/0]).

%-record(seat, {code, row, col, id}).

% <https://stackoverflow.com/a/16986266/5403184> 2020-12-05
read_input_lines(Filename) ->
  {ok,  Data} = file:read_file(Filename),
  binary:split(Data, [<<"\n">>], [global]).

fetch_input(Filename) ->
  lists:filtermap(
    fun (X) ->
        case X of
          <<>> -> false;
          _ -> {true, fetch_int(X)}
        end
    end,
    read_input_lines(Filename)).

fetch_int(String) ->
  case string:to_integer(String) of
    {A, <<>>} -> A % YOLO
  end.

find_target_value_part1_old(Ns, Target) ->
  Partners = lists:map(fun (L) -> lists:nthtail(L, Ns) end, lists:seq(1, length(Ns))),
  Pairs = lists:zipwith(fun (A, Bs) -> lists:map(fun (B) -> {A, B} end, Bs) end, Ns, Partners),
  lists:filtermap(fun ({A, B}) -> case A + B of Target -> {true, {A, B, A * B}}; _ -> false end end, lists:flatten(Pairs)).

% <http://erlang.org/doc/programming_examples/list_comprehensions.html>
% 2020-12-07
find_target_value_part1(Ns, Target) ->
  [ {A, B, A * B} || A <- Ns, B <- Ns, A < B, A + B == Target ].

% <http://erlang.org/doc/programming_examples/list_comprehensions.html>
% 2020-12-07
find_target_value_part2(Ns, Target) ->
  [ {A, B, C, A * B * C} || A <- Ns, B <- Ns, C <- Ns, A < B, B < C, A + B + C == Target ].

test() ->
  Input = fetch_input('test.txt'),
  io:fwrite("test input ~w~n", [Input]),
  io:fwrite("test part1 ~w~n", [find_target_value_part1(Input, 2020)]),
  io:fwrite("test part2 ~w~n", [find_target_value_part2(Input, 2020)]),
  [].

part1() ->
  Input = fetch_input('input.txt'),
  io:fwrite("part1 ~w~n", [find_target_value_part1(Input, 2020)]),
  [].

part2() ->
  Input = fetch_input('input.txt'),
  io:fwrite("part2 ~w~n", [find_target_value_part2(Input, 2020)]),
  [].

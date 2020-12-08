-module(solve).
-export([test/0, part1/0, part2/0]).

-record(r, {lo, hi, c, pass}).

get_parser() ->
  case re:compile("([\\d]+)-([\\d]+) ([a-z]): ([a-z]+)") of
    {ok, MP} -> MP % YOLO
  end.

% <https://stackoverflow.com/a/16986266/5403184> 2020-12-05
read_input_lines(Filename) ->
  {ok,  Data} = file:read_file(Filename),
  binary:split(Data, [<<"\n">>], [global]).

parse(Parser, String) ->
  case re:run(String, Parser, [{capture, all, binary}]) of
    {match, [_, A, B, C, D]} ->
      #r{
         lo=fetch_int(A),
         hi=fetch_int(B),
         c=lists:nth(1, binary_to_list(C)),
         pass=binary_to_list(D)
        }
  end.

fetch_input(Filename) ->
  Parser = get_parser(),
  lists:filtermap(
    fun (X) ->
        case X of
          <<>> -> false;
          _ -> {true, parse(Parser, X)}
        end
    end,
    read_input_lines(Filename)).

fetch_int(String) ->
  case string:to_integer(String) of
    {A, <<>>} -> A % YOLO
  end.

is_valid_part1(Pass) ->
  Matches = length(lists:filtermap(fun (E) -> E == Pass#r.c end, Pass#r.pass)),
  (Pass#r.lo =< Matches) and (Matches =< Pass#r.hi).

is_valid_part2(Pass) ->
  (lists:nth(Pass#r.lo, Pass#r.pass) == Pass#r.c) xor (lists:nth(Pass#r.hi, Pass#r.pass) == Pass#r.c).

test() ->
  Input = fetch_input('test.txt'),
  ValidPart1 = lists:filtermap(fun is_valid_part1/1, Input),
  ValidPart2 = lists:filtermap(fun is_valid_part2/1, Input),
  io:fwrite("test input ~w~n", [Input]),
  io:fwrite("valid part1 ~w~n", [length(ValidPart1)]),
  io:fwrite("valid part2 ~w~n", [length(ValidPart2)]),
  [].

part1() ->
  Input = fetch_input('input.txt'),
  Valids = lists:filtermap(fun is_valid_part1/1, Input),
  io:fwrite("test input ~w~n", [Input]),
  io:fwrite("part1      ~w~n", [length(Valids)]),
  [].

part2() ->
  Input = fetch_input('input.txt'),
  Valids = lists:filtermap(fun is_valid_part2/1, Input),
  io:fwrite("test input ~w~n", [Input]),
  io:fwrite("part2      ~w~n", [length(Valids)]),
  [].

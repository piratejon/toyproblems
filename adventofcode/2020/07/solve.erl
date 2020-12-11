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

parse_rest_of_line(Rest) ->
  case Rest of
    [N | [A | [C | ["bags" | NewRest]]]] -> [{string:join([A, C], "-"), fetch_int(N)}] ++ parse_rest_of_line(NewRest);
    ["no", "other", "bags"] -> [];
    [] -> [];
    _ -> {unparsed, Rest}
  end.

parse_line(Line) ->
  Tokens = [T || T <- re:split(Line, "[ \t\n\.,]+", [{return, list}]), string:length(T) > 0],
  [A | [C | [ "bags" | [ "contain" | Rest ]]]] = [case T of "bag" -> "bags"; _ -> T end || T <- Tokens],
  {string:join([A, C], "-"), dict:from_list(parse_rest_of_line(Rest))}.

fetch_input(Filename) ->
  dict:from_list([parse_line(Line) || Line <- read_input_lines(Filename)]).

fetch_int(String) ->
  case string:to_integer(String) of
    {A, []} -> A;
    _ -> wtf
  end.

a_contains_b(Graph, Haystack, Needle) ->
  case dict:find(Needle, dict:fetch(Haystack, Graph)) of
    {ok, _} -> true;
    _ -> lists:any(fun (H2) -> a_contains_b(Graph, H2, Needle) end, dict:fetch_keys(dict:fetch(Haystack, Graph)))
  end.

solve_part1(Graph, Key) ->
  lists:filter(fun (H) -> a_contains_b(Graph, H, Key) end, dict:fetch_keys(Graph)).

solve_part2(Graph, Bag) ->
  dict:fold(
    fun (Subbag, Count, Acc) ->
        Acc + Count + (Count * solve_part2(Graph, Subbag)) end,
    0, dict:fetch(Bag, Graph)).

test() ->
  TestInputA = fetch_input('test.txt'),
  io:fwrite("Test Input: ~p~n", [dict:fetch_keys(TestInputA)]),
  TestPart1 = solve_part1(TestInputA, "shiny-gold"),
  io:fwrite("Test 1: ~p: ~p~n", [length(TestPart1), TestPart1]),
  TestPart2a = solve_part2(TestInputA, "shiny-gold"),
  io:fwrite("Test 2a: ~p~n", [TestPart2a]),

  TestInputB = fetch_input('test2.txt'),
  TestPart2b = solve_part2(TestInputB, "shiny-gold"),
  io:fwrite("Test 2b: ~p~n", [TestPart2b]),
  [].

part1() ->
  Input = fetch_input('input.txt'),
  Part1 = solve_part1(Input, "shiny-gold"),
  io:fwrite("Part1: ~p~n", [length(Part1)]),
  [].

part2() ->
  Input = fetch_input('input.txt'),
  Part2 = solve_part2(Input, "shiny-gold"),
  io:fwrite("Part 2: ~p~n", [Part2]),
  [].

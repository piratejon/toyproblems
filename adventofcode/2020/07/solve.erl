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
  Input = read_input_lines(Filename),
  Parsed = dict:from_list([parse_line(Line) || Line <- Input]),
  Parsed.

fetch_int(String) ->
  case string:to_integer(String) of
    {A, []} -> A;
    _ -> wtf
  end.

test() ->
  Input = fetch_input('test.txt'),
  io:fwrite("Test Input: ~p~n", [dict:fetch_keys(Input)]),
  [].

part1() ->
  %Input = fetch_input('input.txt'),
  %Part1 = lists:sum([count_group(Group) || Group <- Input]),
  %io:fwrite("Part 1: ~p~n", [Part1]),
  [].

part2() ->
  %Input = fetch_input('input.txt'),
  %Part2 = lists:sum([do_part2(Group) || Group <- Input]),
  %io:fwrite("Part 2: ~p~n", [Part2]),
  [].

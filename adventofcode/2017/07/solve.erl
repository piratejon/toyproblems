-module(solve).
-export([part1/0]). %, part1/1, part2/0, part2/1]).

parse_line(Line) ->
  % turn the line into a name+weight, and optional children
  Parts = re:split(Line, "[ ,\\(\\)\\->]", [{return, list}]),
  [Name, _, Weight | Rest] = Parts,
  Children = lists:filter(fun(X) -> X /= "" end, Rest),
  %io:fwrite("Name: ~s, Weight: ~s, Child Count: ~w, Children: ~s~n", [Name, Weight, length(Children), Children]),
  {Name, Weight, Children}.

read_lines(Filename) ->
  {ok, Data} = file:read_file(Filename),
  Lines = binary:split(Data, [<<"\n">>], [global]),
  lists:map(fun parse_line/1, Lines).

read_the_file(Filename) ->
  Lines = read_lines(Filename),
  io:fwrite("~s~n", [Lines]).

part1() ->
  read_the_file('test.input').

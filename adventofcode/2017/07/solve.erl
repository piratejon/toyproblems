-module(solve).
-export([part1/0]). %, part1/1, part2/0, part2/1]).

parse_line(Line) ->
  % turn the line into a name+weight, and optional children
  Len = string:length(Line),
  if Len > 0 ->
    Parts = re:split(Line, "[ ,\\(\\)\\->]", [{return, list}]),
    [Name, _, WeightStr | Rest] = Parts,
    Children = lists:filter(fun(X) -> X /= "" end, Rest),
    io:fwrite("Name: ~s, Weight: ~s, Child Count: ~w, Children: ~s~n", [Name, WeightStr, length(Children), Children]),
    {ok, Name, string:to_integer(WeightStr), Children};
    true -> {empty}
  end.

build_tree(Items) ->
  lists:foldl(fun({ok, Item, Weight, Children}, Dict) -> dict:store(Item, Weight, Dict) end, dict:new(), Items).

read_lines(Filename) ->
  {ok, Data} = file:read_file(Filename),
  Lines = binary:split(Data, [<<"\n">>], [global]),
  io:fwrite("List: ~w~n", [Lines]),
  dict:to_list(build_tree(lists:map(fun parse_line/1, lists:filter(fun(X) -> string:length(X) > 0 end, Lines)))).

read_the_file(Filename) ->
  Lines = read_lines(Filename),
  io:fwrite("List: ~w~n", [Lines]).

part1() ->
  read_the_file('test.input').

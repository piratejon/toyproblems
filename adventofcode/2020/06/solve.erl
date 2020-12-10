-module(solve).
-export([test/0, part1/0, part2/0]).

% <https://stackoverflow.com/a/16986266/5403184> 2020-12-05
read_input_lines(Filename) ->
  {ok,  Data} = file:read_file(Filename),
  binary:split(Data, [<<"\n\n">>], [global]).

count_group(Group) ->
  Listed = [[X] || X <- binary_to_list(Group), X >= 97, X =< 122],
  %io:fwrite("Group: ~p~n", [Listed]),
  lists:foldl(fun (A, B) -> length(A) + B end, 0, lists:usort(Listed)).

% turn input lines into a group data structure
fetch_input(Filename) ->
  lists:filtermap(
    fun (Group) ->
        case Group of
          <<>> -> false;
          _ -> {true, Group}
        end
    end,
    read_input_lines(Filename)).

fetch_int(String) ->
  case string:to_integer(String) of
    {A, <<>>} -> A % YOLO
  end.

do_part2(Group) ->
  Rsps = [[[S]||S<-binary_to_list(R)] || R <- re:split(Group, "\\n"), string:length(R) > 0],
  Qs = [[X] || X <- lists:usort(lists:flatten(Rsps))],
  RspCount = length(Rsps),
  io:fwrite("rsp: ~p ~p ~p~n", [RspCount, Rsps, Qs]),
  % find Q in Qs in each Rsps
  length(lists:filter(fun (Q) -> lists:all(fun (R) -> lists:any(fun (P) -> P == Q end, R) end, Rsps) end, Qs)).

test() ->
  Input = fetch_input('test.txt'),
  TestPart1 = lists:sum([count_group(Group) || Group <- Input]),
  io:fwrite("Test Part 1: ~p~n", [TestPart1]),

  TestPart2 = lists:sum([do_part2(Group) || Group <- Input]),
  io:fwrite("Test Part 2: ~p~n", [TestPart2]),
  [].

part1() ->
  Input = fetch_input('input.txt'),
  Part1 = lists:sum([count_group(Group) || Group <- Input]),
  io:fwrite("Part 1: ~p~n", [Part1]),
  [].

part2() ->
  Input = fetch_input('input.txt'),
  Part2 = lists:sum([do_part2(Group) || Group <- Input]),
  io:fwrite("Part 2: ~p~n", [Part2]),
  [].

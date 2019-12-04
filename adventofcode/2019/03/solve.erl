-module(solve).
-export([part1/0, part2/0]).

fetch_input_lines(Filename) ->
  {ok, Data} = file:read_file(Filename),
  string:split(unicode:characters_to_list(Data), "\n", all).

string_to_record([Dir | NumChars]) ->
  {Dir, list_to_integer(NumChars)}.

line_to_records(Line) ->
  [string_to_record(G) || G <- string:split(Line, ",", all)].

fetch_input(Filename) ->
  [line_to_records(L) || L <- fetch_input_lines(Filename), length(L) > 0].

chunk_list(List, N) when length(List) > N ->
  [lists:sublist(List, N) | chunk_list(lists:nthtail(N, List), N)];
chunk_list(List, _) -> [List].

impute_at(Program, Position, Value) ->
  lists:sublist(Program, Position) ++ [Value] ++ lists:nthtail(Position + 1, Program).

% <http://erlang.org/pipermail/erlang-questions/2009-June/044173.html>
% 2019-12-03
bfoldl(Fun, Acc0, [Head | Tail]) ->
  case Fun(Head, Acc0) of
    {ok, Acc} -> bfoldl(Fun, Acc, Tail);
    {break, _Acc} = Result -> Result;
    break -> {break, Acc0}
  end;
bfoldl(_, Acc0, []) -> {ok, Acc0}.

add_position({A, B}, {C, D}) ->
  {A + C, B + D}.

move_wire(Step, Positions) ->
  Last = lists:last(Positions),
  Delta = case Step of
    {$U, C} -> {0, C};
    {$D, C} -> {0, -C};
    {$L, C} -> {-C, 0};
    {$R, C} -> {C, 0}
  end,
  Positions ++ [add_position(Last, Delta)].

unroll_wire(Wire) ->
  lists:foldl(fun move_wire/2, [{0, 0}], Wire).

expand_wire(Wire) ->
  lists:foldl(fun expand_wire/2, [{0, 0}], Wire).
expand_wire({Dir, C}, Positions) ->
  Last = lists:last(Positions),
  Steps = lists:seq(1, C),
  Deltas = case Dir of
    $U -> [{0, D} || D <- Steps];
    $D -> [{0, -D} || D <- Steps];
    $L -> [{-D, 0} || D <- Steps];
    $R -> [{D, 0} || D <- Steps]
  end,
  Positions ++ [add_position(Last, Delta) || Delta <- Deltas].

manhattan_distance({A, B}) -> abs(A) + abs(B).

part1() ->
  Wires = fetch_input('input'),
  [WireList1, WireList2] = WireLists = [expand_wire(W) || W <- Wires],
  Wire1 = sets:from_list(WireList1),
  Wire2 = sets:from_list(WireList2),
  %io:fwrite("WireList1 ~w~n", [WireList1]),
  %io:fwrite("WireList2 ~w~n", [WireList2]),
  %io:fwrite("WireSet1 ~w~n", [sets:to_list(Wire1)]),
  %io:fwrite("WireSet2 ~w~n", [sets:to_list(Wire2)]),
  % Crossings = [{A, B} || {A, B} <- Wire1, {C, D} <- Wire2, A =:= C, B =:= D],
  % Crossings = sets:intersection([sets:from_list(W) || W <- WireLists]),
  Crossings = sets:intersection(Wire1, Wire2),
  %io:fwrite("Crossings ~w~n", [sets:to_list(Crossings)]),
  [_ | OrderedCrossings] = lists:sort(fun(A, B) -> manhattan_distance(A) < manhattan_distance(B) end, sets:to_list(Crossings)),
  [{A, B} | _] = OrderedCrossings,
  io:fwrite("part1 ~w~n", [[{A, B}, manhattan_distance({A, B})]]).

part2() -> [].

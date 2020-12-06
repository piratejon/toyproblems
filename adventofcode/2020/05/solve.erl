-module(solve).
-export([test/0, part1/0, part2/0]).

-record(seat, {code, row, col, id}).

% <https://stackoverflow.com/a/16986266/5403184> 2020-12-05
read_input_lines(Filename) ->
  {ok,  Data} = file:read_file(Filename),
  binary:split(Data, [<<"\n">>], [global]).

fetch_input(Filename) ->
  lists:filtermap(
    fun (X) ->
        case X of
          <<>> -> false;
          _ -> {true, binary_to_list(X)}
        end
    end,
    read_input_lines(Filename)).

fetch_int(String) ->
  case string:to_integer(String) of
    {A, <<>>} -> A % YOLO
  end.

parse_input_line(Line) ->
  case binary:split(Line, [<<9>>], [global]) of
    [<<>>] -> false;
    [A, B, C, D] -> {true, #seat{
        code=binary_to_list(A),
        row=fetch_int(B),
        col=fetch_int(C),
        id=fetch_int(D)
    }}
  end.

fetch_test_input(Filename) ->
  lists:filtermap(fun parse_input_line/1, read_input_lines(Filename)).

calc_pos(List, PosVal) ->
  case List of
    [1 | T] -> PosVal + calc_pos(T, 2 * PosVal);
    [0 | T] -> calc_pos(T, 2 * PosVal);
    [] -> 0
  end.

calc_pos(List) -> calc_pos(lists:reverse(List), 1).

translate(Code, One, Zero) ->
  lists:map(
    fun (X) -> case [X] of
        One -> 1;
        Zero -> 0
      end
    end, Code).

calc_seat_id(Code) ->
  {RowSpec, ColSpec} = lists:split(7, Code),
  Row = calc_pos(translate(RowSpec, "B", "F")),
  Col = calc_pos(translate(ColSpec, "R", "L")),
  #seat{code=Code, row=Row, col=Col, id=((Row * 8) + Col)}.

test() ->
  Input = fetch_test_input('test.txt'),
  io:fwrite("test input ~w~n", [Input]),
  Actual = [calc_seat_id(T#seat.code) || T <- Input],
  io:fwrite("calculated ~w~n", [Actual]),
  Results = lists:filtermap(
      fun ({A, B}) ->
          case {A, B} of
            {A, A} -> false; % match, do not report
            {A, B} -> {true, {A, B}} % mismatch, report
          end
      end, lists:zip(Input, Actual)),
  io:fwrite("results ~w~n", [Results]),
  case length(Results) of 0 -> ok end.

sort_seats(A, B) -> A#seat.id > B#seat.id.

part1() ->
  Input = fetch_input("input.txt"),
  %io:fwrite("inputs ~w~n", [Input]),
  Seats = [calc_seat_id(Code) || Code <- Input],
  {Max, _} = lists:split(1, lists:sort(fun sort_seats/2, Seats)),
  io:fwrite("max seat ~w~n", [Max]).

part2() ->
  Input = fetch_input("input.txt"),
  Seats = [calc_seat_id(Code) || Code <- Input],
  Sorted = lists:reverse(lists:sort(fun sort_seats/2, Seats)),
  io:fwrite("long ~w~n", [length(Sorted)]),
  [FirstSeat | Right] = Sorted, io:fwrite("frst ~w~n", [FirstSeat]),
  [LastSeat | LeftX] = lists:reverse(Sorted), io:fwrite("last ~w~n", [LastSeat]),
  Left = lists:reverse(LeftX),
  io:fwrite("long ~w~n", [{length(Left), length(Right)}]),
  Z = lists:map(
      fun ({L, R}) -> {L, R, R#seat.id - L#seat.id} end,
      lists:zip(Left, Right)),
  [A | _ ] = lists:sort(
      fun (X, Y) -> element(3, X) > element(3, Y) end,
      Z),
  % Seat = 2 * (element(2, A) - element(1, A)),
  io:fwrite("seat ~w~n", [{FirstSeat, LastSeat, A}]),
  [].

-module(solve).
-export([part1/0, part2/0]).

check_pass(P) ->
  Diffs = [A - B || {A, B} <- lists:zip(lists:droplast(P), lists:nthtail(1, P))],
  lists:any(fun(A) -> A == 0 end, Diffs) and lists:all(fun(A) -> A =< 0 end, Diffs).

check_pass2(P) ->
  D = [A - B || {A, B} <- lists:zip(lists:droplast(P), lists:nthtail(1, P))],
  D2 = lists:foldl(fun foldl_count_consec/2, [], P),
  %io:fwrite("check2 ~w~n", [[P, D, D2]]),
  lists:any(fun(A) -> A == 0 end, D) and lists:all(fun(A) -> A =< 0 end, D) and lists:any(fun(C) -> element(2, C) == 2 end, D2).

foldl_count_consec(E, A = [H = {He, Hc} | T]) ->
  R = case He of
    E -> [{He, Hc + 1}] ++ T;
    _ -> [{E, 1}] ++ A
  end,
  %io:fwrite("cc ~w~n", [[E, H, T, R]]),
  R;
foldl_count_consec(E, []) ->
  [{E, 1}].

part1() -> % 1611 too high
  [A, B] = [359282, 820401],
  [F | _] = Seq = [X || X <- lists:seq(A, B), check_pass(integer_to_list(X))],
  io:fwrite("part1 ~w~n", [[A, B, length(Seq), F]]).

part2() -> % 148 too low
  [A, B] = [359282, 820401],
  %[A, B] = [112222, 112233],
  Seq = [X || X <- lists:seq(A, B), check_pass2(integer_to_list(X))],
  io:fwrite("part2 ~w~n", [[A, B, length(Seq)]]).

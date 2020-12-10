-module(solve).
-export([test/0, part1/0, part2/0]).

% <https://stackoverflow.com/a/16986266/5403184> 2020-12-05
read_input_lines(Filename) ->
  {ok,  Data} = file:read_file(Filename),
  binary:split(Data, [<<"\n\n">>], [global]).

parse_one_batch(RawBatch) ->
  [
   re:split(Chunk, ":", [{return, list}, {parts,0}])
   || Chunk <- re:split(RawBatch, "[ \n\t]"), string:length(Chunk) > 0
  ].

fetch_input(Filename) ->
  lists:filtermap(
    fun (Batch) ->
        case Batch of
          <<>> -> false;
          _ -> {true, parse_one_batch(Batch)}
        end
    end,
    read_input_lines(Filename)).

fetch_int(String) ->
  case string:to_integer(String) of
    {A, <<>>} -> A % YOLO
  end.

is_passport_part1(Batch) ->
  Keys = lists:sort(lists:map(fun ([K, _]) -> K end, Batch)),
  Required = lists:sort(["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]),
  Optional = lists:sort(Required ++ ["cid"]),
  (Keys == Required) or (Keys == Optional).

validate_year(Value, LBound, UBound) ->
  case re:run(Value, "^\\d+$") of
    {match, _} ->
      case string:to_integer(Value) of
        {A, []} ->
          case (A >= LBound) and (A =< UBound) of
            true -> true;
            false -> {false, cond_fail}
          end;
        _ -> {false, no_parse}
      end;
    _ -> {false, no_re_match}
  end.

validate_height(Value) ->
  case re:run(Value, "^([\\d]+)(cm|in)$", [{capture, all, list}]) of
    {match, [_, N, U]} ->
      case string:to_integer(list_to_binary(N)) of
        {A, <<>>} ->
          case (((U == "cm") and (A >= 150) and (A =< 193)) or ((U == "in") and (A >= 59) and (A =< 76))) of
            true -> true;
            false -> {false, cond_fail, N, U}
          end;
        _ -> {false, no_parse}
      end;
    _ -> {false, no_re_match}
  end.

validate_haircolor(Value) ->
  case re:run(Value, "^\#[\\da-f]{6}$") of
    {match, _} -> true;
    _ -> {false, no_re_match}
  end.

validate_eyecolor(Value) ->
  case lists:any(fun (E) -> Value == E end, ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]) of
    true -> true;
    _ -> {false, no_match}
  end.

validate_pid(Value) ->
  case re:run(Value, "^[\\d]{9}$") of
    {match, _} -> true;
    _ -> {false, no_re_match}
  end.

validate_field([Field, Value]) ->
  %io:fwrite("checking field ~p with value ~p~n", [Field, Value]),
  case Field of
    "byr" -> validate_year(Value, 1920, 2002);
    "iyr" -> validate_year(Value, 2010, 2020);
    "eyr" -> validate_year(Value, 2020, 2030);
    "hgt" -> validate_height(Value);
    "hcl" -> validate_haircolor(Value);
    "ecl" -> validate_eyecolor(Value);
    "pid" -> validate_pid(Value);
    "cid" -> true;
    _ -> {false, unknown_key}
  end.

is_passport_part2(Batch) ->
  case is_passport_part1(Batch) of
    true ->
      Results = [{KeyValue, validate_field(KeyValue)} || KeyValue <- Batch],
      lists:all(fun (R) -> case R of {_, true} -> true; _ -> false end end, Results);
    _ -> false
  end.

test() ->
  Input = fetch_input('test.txt'),
  io:fwrite("Valid passports: ~p~n", [length(lists:filter(fun is_passport_part1/1, Input))]),
  Invalid2 = fetch_input('invalid.txt'),
  io:fwrite("Part2 Test Invalid: ~p/~p~n", [length(lists:filter(fun (X) -> X end, [is_passport_part2(B) || B <- Invalid2])), length(Invalid2)]),
  Valid2 = fetch_input('valid.txt'),
  io:fwrite("Part2 Test Valid: ~p/~p~n", [length(lists:filter(fun (X) -> X end, [is_passport_part2(B) || B <- Valid2])), length(Valid2)]),
  [].

part1() ->
  Input = fetch_input('input.txt'),
  io:fwrite("Valid passports: ~p~n", [length(lists:filter(fun is_passport_part1/1, Input))]),
  [].

part2() ->
  Input = fetch_input('input.txt'),
  io:fwrite("Part2: ~p/~p~n", [length(lists:filter(fun (X) -> X end, [is_passport_part2(B) || B <- Input])), length(Input)]),
  [].

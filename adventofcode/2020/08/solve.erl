-module(solve).
-export([test/0, part1/0, part2/0]).

-record(insn, {op, arg, ip}).
-record(state, {acc, execount}).

% <https://stackoverflow.com/a/16986266/5403184> 2020-12-05
read_input_lines(Filename) ->
  {ok,  Data} = file:read_file(Filename),
  Lines = binary:split(Data, [<<"\n">>], [global]),
  case lists:last(Lines) of
    <<>> -> lists:droplast(Lines);
    _ -> Lines
  end.

parse_line(Line) ->
  [Op, Arg] = re:split(Line, "[ \t]+", [{return, list}]),
  ParsedArg = fetch_int(Arg),
  #insn{
     op=case Op of
          "nop" -> nop;
          "acc" -> acc;
          "jmp" -> jmp
        end,
     arg=ParsedArg
    }.

parse_line_with_location(Line, IP) ->
  Decoded = parse_line(Line),
  Decoded#insn{ip=IP}.

fetch_input(Filename) ->
  Lines = read_input_lines(Filename),
  [parse_line_with_location(Line, IP) || {Line, IP} <- lists:zip(Lines, lists:seq(1, length(Lines)))].

fetch_int(String) ->
  case string:to_integer(String) of
    {A, []} -> A;
    _ -> wtf
  end.

print_state(IP, State, Fin) ->
  io:fwrite("state: IP=~p, tgt=~p, acc=~p, execount=~p~n", [IP, Fin, State#state.acc, dict:to_list(State#state.execount)]).

print_state(IP, State, Fin, Flag) ->
  case Flag of
    true -> print_state(IP, State, Fin);
    _ -> []
  end.

execute(P, IP, State, Fin) ->
  print_state(IP, State, Fin, false),
  case IP of
    Fin -> {terminated, IP, State};
    _ ->
      case dict:fetch(IP, State#state.execount) of
        1 -> {cycle_detected, IP, State};
        0 ->
          StateUpdate = State#state{execount=dict:update_counter(IP, 1, State#state.execount)},
          case lists:nth(IP, P) of
            {insn, nop, _, _} -> execute(P, IP + 1, StateUpdate, Fin);
            {insn, acc, Val, _} -> execute(P, IP + 1, StateUpdate#state{acc=StateUpdate#state.acc + Val}, Fin);
            {insn, jmp, Val, _} -> execute(P, IP + Val, StateUpdate, Fin)
          end
      end
  end.

solve_part1(P) ->
  ProgramLength = length(P),
  State = #state{acc=0, execount=dict:from_list([{K, 0} || K <- lists:seq(1, ProgramLength)])},
  execute(P, 1, State, ProgramLength + 1).

patch_program(P, I) ->
  [case Insn#insn.ip of
      I -> case Insn#insn.op of
             jmp -> Insn#insn{op=nop};
             nop -> Insn#insn{op=jmp}
           end;
      _ -> Insn
    end || Insn <- P].

solve_part2(P) ->
  Trials = [patch_program(P, I) || I <- lists:filtermap(
                                           fun ({Insn, IP}) ->
                                               case Insn#insn.op of
                                                 nop -> {true, IP};
                                                 jmp -> {true, IP};
                                                 _ -> false
                                               end
                                           end, lists:zip(P, lists:seq(1, length(P))))],
  [{IP, State}] = lists:filtermap(
    fun (T) ->
        case solve_part1(T) of
          {terminated, IP, State} -> {true, {IP, State}};
          _ -> false
        end
    end, Trials),
  {IP, State#state.acc}.

test() ->
  TestInput = fetch_input('test.txt'),
  io:fwrite("Test Input: ~p~n", [TestInput]),
  {Result, IP, State} = solve_part1(TestInput),
  io:fwrite("Test Part1: result=~p ip=~p acc=~p~n", [Result, IP, State#state.acc]),
  io:fwrite("Test Part2: ~p~n", [solve_part2(TestInput)]),
  [].

part1() ->
  TestInput = fetch_input('input.txt'),
  {Result, IP, State} = solve_part1(TestInput),
  io:fwrite("Part1: result=~p ip=~p acc=~p~n", [Result, IP, State#state.acc]),
  [].

part2() ->
  Input = fetch_input('input.txt'),
  Part2 = solve_part2(Input),
  io:fwrite("Part 2: ~p~n", [Part2]),
  [].

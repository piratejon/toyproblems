-module(solve).
-export([test/0, part1/0, part2/0]).

-record(insn, {op, arg, di}).
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
     arg=ParsedArg,
     di=case Op of "jmp" -> ParsedArg; _ -> 1 end
    }.

fetch_input(Filename) ->
  [parse_line(Line) || Line <- read_input_lines(Filename)].

fetch_int(String) ->
  case string:to_integer(String) of
    {A, []} -> A;
    _ -> wtf
  end.

print_state(IP, State) ->
  io:fwrite("state: IP=~p, acc=~p, execount=~p~n", [IP, State#state.acc, dict:to_list(State#state.execount)]).

print_state(IP, State, Flag) ->
  case Flag of
    true -> print_state(IP, State);
    _ -> []
  end.

execute(Insns, IP, State) ->
  print_state(IP, State, false),
  case dict:fetch(IP, State#state.execount) of
    1 -> {finished_before, IP, State};
    0 ->
      StateUpdate = State#state{execount=dict:update_counter(IP, 1, State#state.execount)},
      case lists:nth(IP, Insns) of
        {insn, nop, _, D} -> execute(Insns, IP + D, StateUpdate);
        {insn, acc, Val, D} -> execute(Insns, IP + D, StateUpdate#state{acc=StateUpdate#state.acc + Val});
        {insn, jmp, _, D} -> execute(Insns, IP + D, StateUpdate)
      end
  end.

solve_part1(Insns) ->
  State = #state{acc=0, execount=dict:from_list([{K, 0} || K <- lists:seq(1, length(Insns))])},
  execute(Insns, 1, State).

test() ->
  TestInput = fetch_input('test.txt'),
  io:fwrite("Test Input: ~p~n", [TestInput]),
  {Result, IP, State} = solve_part1(TestInput),
  io:fwrite("Test Part1: result=~p ip=~p acc=~p~n", [Result, IP, State#state.acc]),
  [].

part1() ->
  TestInput = fetch_input('input.txt'),
  {Result, IP, State} = solve_part1(TestInput),
  io:fwrite("Part1: result=~p ip=~p acc=~p~n", [Result, IP, State#state.acc]),
  [].

part2() ->
  %Input = fetch_input('input.txt'),
  %Part2 = solve_part2(Input, "shiny-gold"),
  %io:fwrite("Part 2: ~p~n", [Part2]),
  [].

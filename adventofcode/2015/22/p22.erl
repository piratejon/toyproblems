-module(p22).
-export([example_one/0, play_script/2]).
-record(state, {
  player_hp, player_armor, player_mana
  , boss_hp
  , effect_poison , effect_shield, effect_recharge
  , winner
}).

-define(BOSSDAMAGE, 8).

io_wrapper(Left, Right) ->
  io:format(Left, Right).

print_state(State) ->
  io_wrapper("- Player has ~w hit points, ~w armor, ~w mana~n- Boss has ~w hit points~n- Effects: Poison ~w, Shield ~w, Recharge ~w~n",
            [State#state.player_hp, State#state.player_armor, State#state.player_mana, State#state.boss_hp, State#state.effect_poison, State#state.effect_shield, State#state.effect_recharge]
           ).

print_turn_header(State, Who) ->
  io_wrapper("-- ~w turn --~n", [Who]),
  print_state(State).

player_turn(State, Spell) ->
  print_turn_header(State, 'Player'),
  State0 = apply_effects(State),
  io_wrapper("Player casts ~w~n", [Spell]),
  State0.

boss_turn(State) ->
  print_turn_header(State, 'Boss'),
  State0 = apply_effects(State),
  io_wrapper("Boss attacks for ~w damage.~n", [?BOSSDAMAGE]),
  State0#state{player_hp=State0#state.player_hp - ?BOSSDAMAGE}.

% effects appliers, mana was already spent
apply_poison(State) -> State.
apply_shield(State) -> State.
apply_recharge(State) -> State.

% this spends the mana
cast_poison(State) -> State.
cast_shield(State) -> State.
cast_recharge(State) -> State.
cast_missile(State) -> State.
cast_drain(State) -> State.

apply_effects(State) ->
  % i hope it doesn't matter what order the effects apply in?
  apply_poison(
    apply_shield(
      apply_recharge(State)
     )
   ).

set_winner_status(State) ->
  if % player death takes precedence
    State#state.player_hp =< 0 -> State#state{winner=boss};
    State#state.boss_hp =< 0 -> State#state{winner=player};
    true -> State
  end.

play_turn(State, Spell) ->
  State0 = set_winner_status(apply_effects(State)),
  State1 = set_winner_status(if
    State0#state.winner == undefined -> player_turn(State0, Spell);
    true -> State0
  end),
  State2 = set_winner_status(if
    State1#state.winner == undefined -> apply_effects(State1);
    true -> State1
  end),
  State3 = set_winner_status(if
    State2#state.winner == undefined -> boss_turn(State2);
    true -> State2
  end),
  set_winner_status(State3).

play_script(State, []) ->
  print_state(State),
  io_wrapper("*** FIN~n", []),
  State;
play_script(State, [Spell | Rest]) ->
  play_script(play_turn(State, Spell), Rest).

example_one() ->
  State = play_script(
    #state{
       player_hp=10
       , player_armor=0
       , player_mana=250
       , boss_hp=13
      }
    , [
       poison
       , magicmissile
      ]
   ),
  io_wrapper("winner: ~w~n", [State#state.winner])
  .

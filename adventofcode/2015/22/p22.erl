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
  io_wrapper("~n-- ~w turn --~n", [Who]),
  print_state(State).

player_turn(State, Spell) ->
  io_wrapper("Player casts ~w~n", [Spell]),
  State0 = if
      Spell == poison -> cast_poison(State);
      Spell == missile -> cast_missile(State);
      Spell == drain -> cast_drain(State);
      Spell == recharge -> cast_recharge(State);
      Spell == shield -> cast_shield(State)
  end,
  State0.

boss_turn(State) ->
  io_wrapper("Boss attacks for ~w damage.~n", [?BOSSDAMAGE]),
  State#state{player_hp=State#state.player_hp - ?BOSSDAMAGE}.

% effects appliers, mana was already spent, decrements counter
apply_poison(State) ->
  if
    State#state.effect_poison == undefined -> State;
    State#state.effect_poison >= 0 -> State#state{boss_hp=State#state.boss_hp - 3, effect_poison=State#state.effect_poison - 1};
    true -> State
  end.

apply_shield(State) ->
  if
    State#state.effect_shield == undefined -> State;
    State#state.effect_shield == 6 -> State#state{player_armor=State#state.player_armor + 7, effect_shield=State#state.effect_shield - 1};
    State#state.effect_shield > 0 -> State#state{effect_shield=State#state.effect_shield - 1};
    State#state.effect_shield == 0 -> State#state{player_armor=State#state.player_armor - 7, effect_shield=State#state.effect_shield - 1};
    true -> State
  end.

apply_recharge(State) ->
  if
    State#state.effect_recharge == undefined -> State;
    State#state.effect_recharge >= 0 -> State#state{player_mana=State#state.player_mana + 101, effect_recharge=State#state.effect_recharge - 1};
    true -> State
  end.

% this spends the mana
cast_missile(State) ->
  State#state{player_mana=State#state.player_mana - 53, boss_hp=State#state.boss_hp - 4}.

cast_drain(State) ->
  State#state{player_mana=State#state.player_mana - 73, player_hp=State#state.player_hp + 2, boss_hp=State#state.boss_hp - 2}.

cast_shield(State) ->
  State#state{player_mana=State#state.player_mana - 113, effect_shield=6}.

cast_poison(State) ->
  State#state{player_mana=State#state.player_mana - 173, effect_poison=6}.

cast_recharge(State) ->
  State#state{player_mana=State#state.player_mana - 229, effect_recharge=5}.

apply_effects(State) ->
  % i hope it doesn't matter what order the effects apply in?
  apply_poison(
    apply_shield(
      apply_recharge(State)
     )
   ).

set_winner(State) ->
  if % player death takes precedence
    State#state.player_hp =< 0 -> State#state{winner=boss};
    State#state.boss_hp =< 0 -> State#state{winner=player};
    true -> State
  end.

has_winner(State) ->
  State#state.player_hp =< 0 orelse State#state.boss_hp =< 0.

play_turn(State0, Spell) ->
  print_turn_header(State0, 'Player'),

  Has_Winner0 = has_winner(State0),
  State1 = if
    Has_Winner0 -> State0;
    true -> apply_effects(State0)
  end,

  Has_Winner1 = has_winner(State1),
  State2 = if
    Has_Winner1 -> State1;
    true -> player_turn(State1, Spell)
  end,

  print_turn_header(State2, 'Boss'),

  Has_Winner2 = has_winner(State2),
  State3 = if
    Has_Winner2 -> State2;
    true -> apply_effects(State2)
  end,

  Has_Winner3 = has_winner(State3),
  if
    Has_Winner3 -> State3;
    true -> boss_turn(State3)
  end.

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
       , missile
      ]
   ),
  FinalState = set_winner(State),
  io_wrapper("winner: ~w~n", [FinalState#state.winner])
  .


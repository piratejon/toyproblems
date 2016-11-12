"use strict";

var assert = require('assert');

var p22 = require('./p22');

var example_1 = {
    player: new p22.Player({hp: 10, mana: 250, armor: 0}),
    boss: new p22.Player({hp: 13, dmg: 8})
};

var example_2 = {
    player: new p22.Player({hp: 10, mana: 250, armor: 0}),
    boss: new p22.Player({hp: 14, dmg: 8})
};

var real_problem = {
    player: new p22.Player({hp: 50, mana: 500, armor: 0}),
    boss: new p22.Player({hp: 51, dmg: 9})
};

var game = (function (p) {
        return new p22.Game({p1: p.player, p2: p.boss});
    }(real_problem));

var games_played = {};

var REALLY_HIGH_MANA = 999999999;

function play_next_round(in_game, in_mana, level) {
    var game, spell, spent_mana, round_best_mana, header;

    header = Array(level + 1).join('  ') + level.toString() + ' ' + in_game.serialize() + ' ' + in_mana.toString();

    console.log(header);

    if (games_played.hasOwnProperty(in_game.serialize())) {
        console.log('already worked', in_game.serialize(), 'returning', games_played[in_game.serialize()]);
        return games_played[in_game.serialize()];
    }

    round_best_mana = in_mana;

    if ((level % 4) === 1) {
        for (spell in p22.Spells) {
            if (p22.Spells.hasOwnProperty(spell)) {
                if (-1 === in_game.state.effects.map(function (e) { return e.spell; }).indexOf(spell)) {
                    spent_mana = p22.Spells[spell].cost;
                    if (spent_mana <= in_game.state.p1.mana) {
                        game = new p22.Game({state: in_game.serialize()});
                        console.log(header, 'player casts', spell);
                        game.play(spell);
                         console.log('DOT "' + in_game.serialize() + '" -> "' + game.serialize() + '";');

                        if (game.state.p1.hp > 0) { // spell did not kill player
                            if (game.state.p2.hp <= 0) { // player killed boss
                                round_best_mana = Math.min(round_best_mana, in_mana + spent_mana);
                                console.log(header, 'spell killed boss, best mana', round_best_mana);
                            } else { // we can keep drilling
                                console.log(header, game.serialize(), 'game continues');
                                round_best_mana = Math.min(round_best_mana, play_next_round(game, in_mana + spent_mana, level + 1));
                            }
                        } else {
                            console.log(header, 'spell killed player');
                        }
                    }
                } else {
                    console.log(header, 'cannot cast', spell, 'while active');
                }
            }
        }

        games_played[in_game.serialize()] = round_best_mana;
        return round_best_mana;
    }

    // turns 0, 2, 3
    game = new p22.Game({state: in_game.serialize()});
    game.play();

    switch (level % 4) {
    case 0:
        console.log(header, 'apply effects before cast');
        break;
    case 2:
        console.log(header, 'apply effects before attack');
        break;
    case 3:
        console.log(header, 'boss attack');
        break;
    }

     console.log('DOT "' + in_game.serialize() + '" -> "' + game.serialize() + '";');

    if (game.state.p1.hp <= 0) {
        console.log(header, 'player killed');
        games_played[in_game.serialize()] = round_best_mana;
        return REALLY_HIGH_MANA;
    }

    if (game.state.p2.hp <= 0) {
        console.log(header, 'boss killed, best mana', round_best_mana);
        games_played[in_game.serialize()] = round_best_mana;
        return round_best_mana;
    }

    console.log(header, 'game continues', game.serialize());
    round_best_mana = play_next_round(game, round_best_mana, level + 1);
    games_played[in_game.serialize()] = round_best_mana;
    return round_best_mana;
}

 console.log('DOT digraph G {');
console.log(play_next_round(game, 0, 0));
 console.log('DOT }');


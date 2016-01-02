"use strict";

var assert = require('assert');

var p22 = require('./p22');

var player = new p22.Player({hp: 50, mana: 500, armor: 0});
var boss = new p22.Player({hp: 51, dmg: 9});

var game = new p22.Game({p1: player, p2: boss});

function clone_game(src) {
    var p1, p2, dst, p, i, new_effect;

    p1 = new p22.Player({});
    p2 = new p22.Player({});

    for (p in src.p1) {
        if (src.p1.hasOwnProperty(p)) {
            p1[p] = src.p1[p];
        }
    }

    for (p in src.p2) {
        if (src.p2.hasOwnProperty(p)) {
            p2[p] = src.p2[p];
        }
    }

    dst = new p22.Game({p1: p1, p2: p2});

    for (i = 0; i < src.effects.length; i += 1) {
        new_effect = {};
        for (p in src.effects[i]) {
            if (src.effects[i].hasOwnProperty(p)) {
                if (p === 'caster' || p === 'target') {
                    if (src.effects[i][p] === src.p1) {
                        new_effect[p] = p1;
                    } else if (src.effects[i][p] === src.p2) {
                        new_effect[p] = p2;
                    } else {
                        assert(false);
                    }
                } else {
                    new_effect[p] = src.effects[i][p];
                }
            }
        }
        dst.effects.push(new_effect);
    }

    return dst;
}

var games_played = {};

// returns mana spent
function play_next_round(in_game, gamestr, spent_mana, level) {
    var spell, min_spent_mana, g, current_spell_cost;

    min_spent_mana = 99999999999;

    if (games_played.hasOwnProperty(in_game)) {
        return games_played[in_game];
    }

    in_game.apply_effects();

    if (in_game.p1.hp > 0) {
        if (in_game.p2.hp <= 0) {
            console.log('we won', spent_mana);
            games_played[in_game] = spent_mana;
            return spent_mana;
        }

        // try each spell
        for (spell in p22.Spells) {
            if (p22.Spells.hasOwnProperty(spell)) {
                current_spell_cost = p22.Spells[spell].cost;
                if (in_game.p1.mana >= current_spell_cost) {
                    g = clone_game(in_game);
                    console.log(level, 'casting', spell, current_spell_cost, spent_mana + current_spell_cost);

                    g.cast({caster: g.p1, target: g.p2, spell: spell});

                    if (g.p1.hp > 0) {
                        if (g.p2.hp <= 0) {
                            // we won, preserve the score
                            min_spent_mana = [min_spent_mana, spent_mana + current_spell_cost].sort()[0];
                            console.log('we won', min_spent_mana);
                        } else {
                            // did not win; apply effects
                            g.apply_effects();

                            if (g.p1.hp > 0) {
                                if (g.p2.hp <= 0) {
                                    // we won, preserve the score
                                    min_spent_mana = [min_spent_mana, spent_mana + current_spell_cost].sort()[0];
                                    console.log('we won', min_spent_mana);
                                } else {
                                    g.attack({player: g.p2, target: g.p1});

                                    if (g.p1.hp > 0) {
                                        min_spent_mana = [min_spent_mana, play_next_round(g, spent_mana + current_spell_cost, level + 1)].sort()[0];
                                    }
                                }
                            }
                        }
                    } // else p1 died, keep trying
                }
            }
        }
    }

    games_played[in_game] = min_spent_mana;
    return min_spent_mana;
}

console.log(play_next_round(game, 0, 0));


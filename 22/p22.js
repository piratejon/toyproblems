"use strict";

module.exports = (function () {
    var Player, GameState, Spells, Game, assert;

    assert = require('assert');

    Spells = {
        'Magic Missile': {
            cost: 53,
            type: 'instant',
            impact: [
                {
                    player: 'target',
                    stat: 'hp',
                    type: 'real',
                    delta: -4
                }
            ]
        },
        'Drain': {
            cost: 73,
            type: 'instant',
            impact: [
                {
                    player: 'target',
                    stat: 'hp',
                    type: 'real',
                    delta: -2
                },
                {
                    player: 'caster',
                    stat: 'hp',
                    type: 'real',
                    delta: 2
                }
            ]
        },
        'Shield': {
            cost: 113,
            type: 'effect',
            duration: 6,
            impact: [
                {
                    player: 'caster',
                    stat: 'armor',
                    type: 'effective',
                    delta: 7
                }
            ]
        },
        'Poison': {
            cost: 173,
            type: 'effect',
            duration: 6,
            impact: [
                {
                    player: 'target',
                    stat: 'hp',
                    type: 'real',
                    delta: -3
                }
            ]
        },
        'Recharge': {
            cost: 229,
            type: 'effect',
            duration: 5,
            impact: [
                {
                    player: 'caster',
                    stat: 'mana',
                    type: 'real',
                    delta: 101
                }
            ]
        }
    };

    Player = function (args) {
        var i, player_properties;

        player_properties = ['hp', 'armor', 'dmg', 'mana'];

        for (i = 0; i < player_properties.length; i += 1) {
            if (args.hasOwnProperty(player_properties[i])) {
                this[player_properties[i]] = args[player_properties[i]];
            }
        }

        if (this.hasOwnProperty('armor') && !this.hasOwnProperty('effective_armor')) {
            this.effective_armor = this.armor;
        }

        delete this.i;
        delete this.player_properties;
    };

    GameState = function (args) {
        var i, game_properties;

        game_properties = ['p1', 'p2'];

        for (i = 0; i < game_properties.length; i += 1) {
            if (args.hasOwnProperty(game_properties[i])) {
                this[game_properties[i]] = args[game_properties[i]];
            }
        }

        this.effects = [];

        delete this.i;
        delete this.game_properties;
    };

    GameState.prototype.apply_effect_impact = function (player, impact) {
        if (impact.type === 'real') {
            player[impact.stat] += impact.delta;
        } else if (impact.type === 'effective') {
            player['effective_' + impact.stat] = player[impact.stat] + impact.delta;
        } else {
            assert(false);
        }
    };

    GameState.prototype.apply_effects = function () {
        var i, j, effect, spell, new_effects;

        new_effects = [];

        for (i = 0; i < this.effects.length; i += 1) {
            effect = this.effects[i];
            effect.ttl -= 1;
            if (effect.ttl >= 0) {
                new_effects.push(effect);
                spell = Spells[effect.spell];
                assert.equal(spell.type, 'effect');
                for (j = 0; j < spell.impact.length; j += 1) {
                    this.apply_effect_impact(effect[spell.impact[j].player], spell.impact[j]);
                }
            }
        }

        this.effects = new_effects;
    };

    GameState.prototype.attack = function (args) {
        var effective_dmg;
        effective_dmg = ([1, args.target.hasOwnProperty('effective_armor') ? (args.player.dmg - args.target.effective_armor) : (args.player.dmg - args.target.armor)].sort())[1];
        args.target.hp -= effective_dmg;
    };

    GameState.prototype.cast = function (args) {
        var i, props, casting, spell;

        casting = {};

        props = ['caster', 'target', 'spell'];

        for (i = 0; i < props.length; i += 1) {
            if (args.hasOwnProperty(props[i])) {
                casting[props[i]] = args[props[i]];
            }
        }

        // decrement caster mana
        casting.caster.mana -= Spells[casting.spell].cost;

        // set duration and append to effects if necessary
        if (Spells[args.spell].type === 'effect') {
            casting.ttl = Spells[args.spell].duration;
            this.effects.push(casting);
        } else if (Spells[args.spell].type === 'instant') {
            spell = Spells[args.spell];
            for (i = 0; i < spell.impact.length; i += 1) {
                this.apply_effect_impact(args[spell.impact[i].player], spell.impact[i]);
            }
        } else {
            assert(false);
        }
    };

    Game = function (args) {
        this.state = new GameState(args);
        this.turn = 0;
    };

    Game.prototype.play = function (spell_name) {
        switch (this.turn % 4) {
        case 0:
            this.state.apply_effects();
            break;
        case 1:
            this.state.cast({caster: this.state.p1, target: this.state.p2, spell: spell_name});
            break;
        case 2:
            this.state.apply_effects();
            break;
        case 3:
            this.state.attack({player: this.state.p2, target: this.state.p1});
            break;
        }

        this.turn += 1;
    };

    Game.prototype.serialize = function () {
        var i, pieces;

        // serialized string is:
        // turn%4,p1.hp,p1.armor,p1.effective_armor,p1.mana,p2.hp,p2.dmg,effect_count[,e1.spell,e1.ttl,e1.caster[,e1.target]]

        pieces = [];
        pieces.push(this.turn % 4);
        pieces.push(this.state.p1.hp);
        pieces.push(this.state.p1.armor);
        pieces.push(this.state.p1.effective_armor);
        pieces.push(this.state.p1.mana);
        pieces.push(this.state.p2.hp);
        pieces.push(this.state.p2.dmg);
        pieces.push(this.state.effects.length);

        for (i = 0; i < this.state.effects.length; i += 1) {
            pieces.push(this.state.effects[i].spell[0]);
            pieces.push(this.state.effects[i].ttl);
            pieces.push(this.state.effects[i].caster === this.state.p1 ? '1' : '2');
            if (this.state.effects[i].hasOwnProperty('target')) {
                pieces.push(this.state.effects[i].target === this.state.p1 ? '1' : '2');
            } else {
                pieces.push('-');
            }
        }

        return pieces.join();
    };

    return {
        Player: Player,
        GameState: GameState,
        Game: Game,
        Spells: Spells
    };
}());


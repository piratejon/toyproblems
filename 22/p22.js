"use strict";

module.exports = (function () {
    var Player, Game, Spells;

    Spells = {
        'Magic Missile': {
            cost: 53,
            type: 'instant',
            impact: [
                {
                    player: 'target',
                    stat: 'dmg',
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
                    stat: 'dmg',
                    delta: -2
                },
                {
                    player: 'caster',
                    stat: 'hp',
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

        delete this.i;
        delete this.player_properties;
    };

    Game = function (args) {
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

    Game.prototype.effective_armor = function (player) {
        return player.armor;
    };

    Game.prototype.apply_effects = function () {
    };

    Game.prototype.cast = function (args) {
        var i, props, casting;

        casting = {};

        props = ['caster', 'target', 'spell'];

        for (i = 0; i < props.length; i += 1) {
            if (args.hasOwnProperty(props[i])) {
                casting[props[i]] = args[props[i]];
            }
        }

        casting.ttl = Spells[args.spell].duration;

        this.effects.push(casting);
    };

    return {
        Player: Player,
        Game: Game
    };
}());


var assert = require('assert');
var p22 = require('./p22');

describe('p22_part1', function () {
    describe('example1', function () {
        it('initializes a player', function () {
            var player, boss;

            player = new p22.Player({hp: 10, armor: 0, mana: 250, dmg: 0});
            boss = new p22.Player({hp: 13, armor: 0, mana: 0, dmg: 8});

            assert.equal(player.hp, 10);
            assert.equal(player.armor, 0);
            assert.equal(player.mana, 250);
            assert.equal(player.dmg, 0);

            assert.equal(boss.hp, 13);
            assert.equal(boss.armor, 0);
            assert.equal(boss.mana, 0);
            assert.equal(boss.dmg, 8);
        });

        it('initializes a gamestate', function () {
            var gamestate, player, boss;

            player = new p22.Player({hp: 10, armor: 0, mana: 250, dmg: 0});
            boss = new p22.Player({hp: 13, armor: 0, mana: 0, dmg: 8});

            gamestate = new p22.GameState({p1: player, p2: boss});

            assert.equal(gamestate.p1, player);
            assert.equal(gamestate.p2, boss);
        });

        it('plays the first example by gamestate state only', function () {
            var gamestate, player, boss;

            player = new p22.Player({hp: 10, armor: 0, mana: 250, dmg: 0});
            boss = new p22.Player({hp: 13, armor: 0, mana: 0, dmg: 8});

            gamestate = new p22.GameState({p1: player, p2: boss});

            // initial state
            assert.equal(gamestate.p1.hp, 10);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 250);
            assert.equal(gamestate.p2.hp, 13);
            assert.deepEqual(gamestate.effects, []);

            // no effects at first
            gamestate.apply_effects();

            // turn 1
            gamestate.cast({caster: gamestate.p1, spell: 'Poison', target: gamestate.p2});
            assert.deepEqual(gamestate.effects, [{spell: 'Poison', ttl: 6, caster: gamestate.p1, target: gamestate.p2}]);
            assert.equal(gamestate.p1, gamestate.effects[0].caster);
            assert.equal(gamestate.p2, gamestate.effects[0].target);

            // turn 2
            assert.equal(gamestate.p1.hp, 10);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 77);
            assert.equal(gamestate.p2.hp, 13);
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 10);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 77);
            assert.equal(gamestate.p2.hp, 10);
            gamestate.attack({player: gamestate.p2, target: gamestate.p1});

            // turn 3
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 77);
            assert.equal(gamestate.p2.hp, 10);
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 77);
            assert.equal(gamestate.p2.hp, 7);
            gamestate.cast({caster: gamestate.p1, target: gamestate.p2, spell: 'Magic Missile'});

            // turn 4
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 24);
            assert.equal(gamestate.p2.hp, 3);
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 24);
            assert.equal(gamestate.p2.hp, 0);
        });

        it('plays the second example', function () {
            var gamestate, player, boss;

            player = new p22.Player({hp: 10, armor: 0, mana: 250, dmg: 0});
            boss = new p22.Player({hp: 14, armor: 0, mana: 0, dmg: 8});

            gamestate = new p22.GameState({p1: player, p2: boss});

            // initial state
            assert.equal(gamestate.p1.hp, 10);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 250);
            assert.equal(gamestate.p2.hp, 14);
            assert.deepEqual(gamestate.effects, []);

            // no effects at first
            gamestate.apply_effects();

            // turn 1
            gamestate.cast({caster: gamestate.p1, spell: 'Recharge'});
            assert.equal(gamestate.p1.hp, 10);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 21);
            assert.equal(gamestate.p2.hp, 14);
            assert.deepEqual(gamestate.effects, [{spell: 'Recharge', ttl: 5, caster: gamestate.p1}]);

            // turn 2
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 10);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 122);
            assert.equal(gamestate.p2.hp, 14);
            assert.deepEqual(gamestate.effects, [{spell: 'Recharge', ttl: 4, caster: gamestate.p1}]);
            gamestate.attack({player: gamestate.p2, target: gamestate.p1});
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 122);
            assert.equal(gamestate.p2.hp, 14);

            // turn 3
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 223);
            assert.equal(gamestate.p2.hp, 14);
            assert.deepEqual(gamestate.effects, [{spell: 'Recharge', ttl: 3, caster: gamestate.p1}]);
            gamestate.cast({caster: gamestate.p1, spell: 'Shield'});
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.mana, 110);
            assert.equal(gamestate.p2.hp, 14);
            assert.deepEqual(gamestate.effects, [
                {spell: 'Recharge', ttl: 3, caster: gamestate.p1},
                {spell: 'Shield', ttl: 6, caster: gamestate.p1}
            ]);

            // turn 4
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 211);
            assert.equal(gamestate.p2.hp, 14);
            assert.deepEqual(gamestate.effects, [
                {spell: 'Recharge', ttl: 2, caster: gamestate.p1},
                {spell: 'Shield', ttl: 5, caster: gamestate.p1}
            ]);
            gamestate.attack({player: gamestate.p2, target: gamestate.p1});
            assert.equal(gamestate.p1.hp, 1);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 211);
            assert.equal(gamestate.p2.hp, 14);

            // turn 5
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 1);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 312);
            assert.equal(gamestate.p2.hp, 14);
            assert.deepEqual(gamestate.effects, [
                {spell: 'Recharge', ttl: 1, caster: gamestate.p1},
                {spell: 'Shield', ttl: 4, caster: gamestate.p1}
            ]);
            gamestate.cast({caster: gamestate.p1, target: gamestate.p2, spell: 'Drain'});
            assert.equal(gamestate.p1.hp, 3);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 239);
            assert.equal(gamestate.p2.hp, 12);

            // turn 6
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 3);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 340);
            assert.equal(gamestate.p2.hp, 12);
            assert.deepEqual(gamestate.effects, [
                {spell: 'Recharge', ttl: 0, caster: gamestate.p1},
                {spell: 'Shield', ttl: 3, caster: gamestate.p1}
            ]);
            gamestate.attack({player: gamestate.p2, target: gamestate.p1});
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 340);
            assert.equal(gamestate.p2.hp, 12);

            // turn 7
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 340);
            assert.equal(gamestate.p2.hp, 12);
            assert.deepEqual(gamestate.effects, [
                {spell: 'Shield', ttl: 2, caster: gamestate.p1}
            ]);
            gamestate.cast({caster: gamestate.p1, target: gamestate.p2, spell: 'Poison'});
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 167);
            assert.equal(gamestate.p2.hp, 12);

            // turn 8
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 2);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 167);
            assert.equal(gamestate.p2.hp, 9);
            assert.deepEqual(gamestate.effects, [
                {spell: 'Shield', ttl: 1, caster: gamestate.p1},
                {spell: 'Poison', ttl: 5, caster: gamestate.p1, target: gamestate.p2}
            ]);
            gamestate.attack({player: gamestate.p2, target: gamestate.p1});
            assert.equal(gamestate.p1.hp, 1);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 167);
            assert.equal(gamestate.p2.hp, 9);

            // turn 9
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 1);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 167);
            assert.equal(gamestate.p2.hp, 6);
            assert.deepEqual(gamestate.effects, [
                {spell: 'Shield', ttl: 0, caster: gamestate.p1},
                {spell: 'Poison', ttl: 4, caster: gamestate.p1, target: gamestate.p2}
            ]);
            gamestate.cast({caster: gamestate.p1, target: gamestate.p2, spell: 'Magic Missile'});
            assert.equal(gamestate.p1.hp, 1);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 7);
            assert.equal(gamestate.p1.mana, 114);
            assert.equal(gamestate.p2.hp, 2);

            // turn 10
            gamestate.apply_effects();
            assert.equal(gamestate.p1.hp, 1);
            assert.equal(gamestate.p1.armor, 0);
            assert.equal(gamestate.p1.effective_armor, 0);
            assert.equal(gamestate.p1.mana, 114);
            assert.equal(gamestate.p2.hp, -1);
            assert.deepEqual(gamestate.effects, [
                {spell: 'Poison', ttl: 3, caster: gamestate.p1, target: gamestate.p2}
            ]);
        });
    });

    describe('plays the examples with turn tracking', function () {
        it ('plays the first example', function () {
            var player, boss, game, player_spell_stream;
            player = new p22.Player({hp: 10, armor: 0, mana: 250});
            boss = new p22.Player({hp: 13, armor: 0, dmg: 8});

            game = new p22.Game({p1: player, p2: boss});

            player_spell_stream = ['Poison', 'Magic Missile'];

            assert.equal(game.turn, 0);
            game.play(); // apply effects before player turn
            assert.equal(game.turn, 1);
            game.play('Poison'); // player spell
            assert.equal(game.turn, 2);
            game.play(); // apply effects before boss turn
            assert.equal(game.turn, 3);
            game.play();  // boss attack
            assert.equal(game.turn, 4);
            game.play(); // apply effects before player turn
            assert.equal(game.turn, 5);
            game.play('Magic Missile'); // player spell
            assert.equal(game.turn, 6);
            game.play(); // apply effects before boss turn
            assert.equal(game.turn, 7);

            assert.equal(game.state.p1.hp, 2);
            assert.equal(game.state.p1.armor, 0);
            assert.equal(game.state.p1.mana, 24);
            assert.equal(game.state.p2.hp, 0);
        });

        it ('plays the first example with serialization', function () {
            var player, boss, game;

            // player plays: Poison, Magic Missile
            player = new p22.Player({hp: 10, armor: 0, mana: 250});
            boss = new p22.Player({hp: 13, armor: 0, dmg: 8});

            game = new p22.Game({p1: player, p2: boss});

            assert.equal(game.serialize(), '0,10,0,0,250,13,8,0');

            assert.equal(game.turn, 0);
            game.play(); // apply effects before player turn
            assert.equal(game.serialize(), '1,10,0,0,250,13,8,0');
            assert.equal(game.turn, 1);
            game.play('Poison'); // player spell
            assert.equal(game.serialize(), '2,10,0,0,77,13,8,1,P,6,1,2');
            assert.equal(game.turn, 2);
            game.play(); // apply effects before boss turn
            assert.equal(game.serialize(), '3,10,0,0,77,10,8,1,P,5,1,2');
            assert.equal(game.turn, 3);
            game.play();  // boss attack
            assert.equal(game.serialize(), '0,2,0,0,77,10,8,1,P,5,1,2');
            assert.equal(game.turn, 4);
            game.play(); // apply effects before player turn
            assert.equal(game.serialize(), '1,2,0,0,77,7,8,1,P,4,1,2');
            assert.equal(game.turn, 5);
            game.play('Magic Missile'); // player spell
            assert.equal(game.serialize(), '2,2,0,0,24,3,8,1,P,4,1,2');
            assert.equal(game.turn, 6);
            game.play(); // apply effects before boss turn
            assert.equal(game.serialize(), '3,2,0,0,24,0,8,1,P,3,1,2');
            assert.equal(game.turn, 7);

            assert.equal(game.state.p1.hp, 2);
            assert.equal(game.state.p1.armor, 0);
            assert.equal(game.state.p1.mana, 24);
            assert.equal(game.state.p2.hp, 0);
        });

        it ('plays the second example with serialization', function () {
            var player, boss, game;

            // player plays: Recharge, Shield, Drain, Poison, Magic Missile
            player = new p22.Player({hp: 10, armor: 0, mana: 250});
            boss = new p22.Player({hp: 14, armor: 0, dmg: 8});

            game = new p22.Game({p1: player, p2: boss});

            assert.equal(game.serialize(), '0,10,0,0,250,14,8,0');
            game.play(); // apply effects before player turn
            assert.equal(game.serialize(), '1,10,0,0,250,14,8,0');
            game.play('Recharge'); // player spell
            assert.equal(game.serialize(), '2,10,0,0,21,14,8,1,R,5,1,2');
            game.play(); // apply effects before boss turn
            assert.equal(game.serialize(), '3,10,0,0,122,14,8,1,R,4,1,2');
            game.play();  // boss attack

            assert.equal(game.serialize(), '0,2,0,0,122,14,8,1,R,4,1,2');
            game.play(); // apply effects before player turn
            assert.equal(game.serialize(), '1,2,0,0,223,14,8,1,R,3,1,2');
            game.play('Shield'); // player spell
            assert.equal(game.serialize(), '2,2,0,0,110,14,8,2,R,3,1,2,S,6,1,2');
            game.play(); // apply effects before boss turn
            assert.equal(game.serialize(), '3,2,0,7,211,14,8,2,R,2,1,2,S,5,1,2');
            game.play(); // boss attack

            assert.equal(game.serialize(), '0,1,0,7,211,14,8,2,R,2,1,2,S,5,1,2');
            game.play(); // apply effects before player turn
            assert.equal(game.serialize(), '1,1,0,7,312,14,8,2,R,1,1,2,S,4,1,2');
            game.play('Drain');
            assert.equal(game.serialize(), '2,3,0,7,239,12,8,2,R,1,1,2,S,4,1,2');
            game.play(); // apply effect before boss turn
            assert.equal(game.serialize(), '3,3,0,7,340,12,8,2,R,0,1,2,S,3,1,2');
            game.play(); // boss attack

            assert.equal(game.serialize(), '0,2,0,7,340,12,8,2,R,0,1,2,S,3,1,2');
            game.play(); // apply effects before player turn
            assert.equal(game.serialize(), '1,2,0,7,340,12,8,1,S,2,1,2');
            game.play('Poison');
            assert.equal(game.serialize(), '2,2,0,7,167,12,8,2,S,2,1,2,P,6,1,2');
            game.play(); // apply effect before boss turn
            assert.equal(game.serialize(), '3,2,0,7,167,9,8,2,S,1,1,2,P,5,1,2');
            game.play(); // boss attack

            assert.equal(game.serialize(), '0,1,0,7,167,9,8,2,S,1,1,2,P,5,1,2');
            game.play(); // apply effects before player turn
            assert.equal(game.serialize(), '1,1,0,7,167,6,8,2,S,0,1,2,P,4,1,2');
            game.play('Magic Missile');
            assert.equal(game.serialize(), '2,1,0,7,114,2,8,2,S,0,1,2,P,4,1,2');
            game.play(); // apply effects before boss turn
            assert.equal(game.serialize(), '3,1,0,0,114,-1,8,1,P,3,1,2');

            assert.equal(game.state.p1.hp, 1);
            assert.equal(game.state.p1.armor, 0);
            assert.equal(game.state.p1.effective_armor, 0);
            assert.equal(game.state.p1.mana, 114);
            assert.equal(game.state.p2.hp, -1);
        });
    });

    describe('exercise the deserializer', function () {
        it('deserializes a state', function () {
            var game;
            game = new p22.Game({state: '3,1,0,0,114,-1,8,1,P,3,1,2'});
            assert.equal(game.state.p1.hp, 1);
            assert.equal(game.state.p1.armor, 0);
            assert.equal(game.state.p1.effective_armor, 0);
            assert.equal(game.state.p1.mana, 114);
            assert.equal(game.state.p2.hp, -1);
        });

        it('effectively deep-copies the state', function () {
            var game0, game1;
            game0 = new p22.Game({state: '0,1,0,7,167,9,8,2,S,1,1,2,P,5,1,2'});
            game1 = new p22.Game({state: game0.serialize()});

            assert.equal(game0.serialize(), game1.serialize());

            game0.play();
            assert.notEqual(game0.serialize(), game1.serialize());
            game1.play();
            assert.equal(game0.serialize(), game1.serialize());

            game0.play('Magic Missile');
            assert.notEqual(game0.serialize(), game1.serialize());
            game0.play();
            assert.notEqual(game0.serialize(), game1.serialize());

            game1.play('Magic Missile');
            assert.notEqual(game0.serialize(), game1.serialize());
            game1.play();
            assert.equal(game0.serialize(), game1.serialize());
        });
    });
});


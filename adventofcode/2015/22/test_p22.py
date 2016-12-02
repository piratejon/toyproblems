#!/usr/bin/python3

import unittest

import p22

class P22Test(unittest.TestCase):

    def test_part1_example1_game(self):
        player = p22.Player(hp=10, mana=250)
        boss = p22.Player(hp=13, dmg=8)

        game = p22.Game(p1=player, p2=boss)

# initial game state
        self.assertEqual(game.p1.hp, 10)
        self.assertEqual(game.p1.armor, 0)
        self.assertEqual(game.p1.mana, 250)
        self.assertEqual(game.p2.hp, 13)
# turn 1 happens
        game.apply_effects()
        self.assertTrue(game.cast(player=game.p1, spell='Poison', target=game.p2))

# turn 2 state
        self.assertEqual(game.p1.hp, 10)
        self.assertEqual(game.p1.armor, 0)
        self.assertEqual(game.p1.mana, 77)
        self.assertEqual(game.p2.hp, 13)
        self.assertEqual(game.effects[0], {
            'name': 'Poison',
            'timer': 5,
            'caster': game.p1,
            'target': game.p2
            })
# turn 2 happens
        game.apply_effects()
        self.assertTrue(game.attack(player=game.p2, target=game.p1))


# turn 3 state
        self.assertEqual(game.p1.hp, 2)
        self.assertEqual(game.p1.armor, 0)
        self.assertEqual(game.p1.mana, 77)
        self.assertEqual(game.p2.hp, 10)
        self.assertEqual(game.effects[0], {
            'name': 'Poison',
            'timer': 4,
            'caster': game.p1,
            'target': game.p2
            })
# turn 3 happens
        game.apply_effects()
        self.assertTrue(game.cast(player=game.p1, spell='Magic Missile', target=game.p2))

# turn 4 state
        self.assertEqual(game.p1.hp, 2)
        self.assertEqual(game.p1.armor, 0)
        self.assertEqual(game.p1.mana, 24)
        self.assertEqual(game.p2.hp, 3)
        self.assertEqual(game.effects[0], {
            'name': 'Poison',
            'timer': 3,
            'caster': game.p1,
            'target': game.p2
            })
# turn 4
        game.apply_effects()
        self.assertFalse(game.attack(player=game.p2, target=game.p1))
        self.assertEqual(game.p1.hp, 2)
        self.assertEqual(game.p1.armor, 0)
        self.assertEqual(game.p1.mana, 24)
        self.assertEqual(game.p2.hp, 0)
        self.assertEqual(game.effects[0], {
            'name': 'Poison',
            'timer': 2,
            'caster': game.p1,
            'target': game.p2
            })

    def test_part1_example2_game(self):
        player = p22.Player(hp=10, mana=250)
        boss = p22.Player(hp=14, dmg=8)

        game = p22.Game(p1=player, p2=boss)

# initial game state
        self.assertEqual(game.p1.hp, 10)
        self.assertEqual(game.p1.armor, 0)
        self.assertEqual(game.p1.mana, 250)
        self.assertEqual(game.p2.hp, 14)
# turn 1 happens
        game.apply_effects()
        self.assertTrue(game.cast(player=game.p1, spell='Recharge'))

# turn 2 state
        self.assertEqual(game.p1.hp, 10)
        self.assertEqual(game.p1.armor, 0)
        self.assertEqual(game.p1.mana, 21)
        self.assertEqual(game.p2.hp, 14)
        self.assertEqual(game.effects[0], {
            'name': 'Recharge',
            'timer': 4,
            'caster': game.p1,
            'target': None
            })
# turn 2 happens
        game.apply_effects()
        self.assertTrue(game.attack(player=game.p2, target=game.p1))

# turn 3 state
        self.assertEqual(game.p1.hp, 2)
        self.assertEqual(game.p1.armor, 0)
        self.assertEqual(game.p1.mana, 122)
        self.assertEqual(game.p2.hp, 14)
        self.assertEqual(game.effects[0], {
            'name': 'Recharge',
            'timer': 3,
            'caster': game.p1,
            'target': None
            })
# turn 3 happens
        game.apply_effects()
        self.assertTrue(game.cast(player=game.p1, spell='Shield'))

# turn 4 state
        self.assertEqual(game.p1.hp, 2)
        self.assertEqual(game.p1.armor, 7)
        self.assertEqual(game.p1.mana, 110)
        self.assertEqual(game.p2.hp, 14)
        self.assertEqual(game.effects, [{
            'name': 'Recharge',
            'timer': 2,
            'caster': game.p1,
            'target': None
        }, {
            'name': 'Shield',
            'timer': 5,
            'caster': game.p1,
            'target': None
        }])
# turn 4
        game.apply_effects()
        self.assertTrue(game.attack(player=game.p2, target=game.p1))

# turn 5 state
        self.assertEqual(game.p1.hp, 1)
        self.assertEqual(game.p1.armor, 7)
        self.assertEqual(game.p1.mana, 211)
        self.assertEqual(game.p2.hp, 14)
        self.assertEqual(game.effects, [{
            'name': 'Recharge',
            'timer': 1,
            'caster': game.p1,
            'target': None
        }, {
            'name': 'Shield',
            'timer': 4,
            'caster': game.p1,
            'target': None
        }])
# turn 5
        game.apply_effects()
        self.assertTrue(game.cast(player=game.p1, spell='Drain', target=game.p2))

# turn 6 state
        self.assertEqual(game.p1.hp, 3)
        self.assertEqual(game.p1.armor, 7)
        self.assertEqual(game.p1.mana, 239)
        self.assertEqual(game.p2.hp, 12)
        self.assertEqual(game.effects, [{
            'name': 'Recharge',
            'timer': 0,
            'caster': game.p1,
            'target': None
        }, {
            'name': 'Shield',
            'timer': 3,
            'caster': game.p1,
            'target': None
        }])
# turn 6
        game.apply_effects()
        self.assertTrue(game.attack(player=game.p2, target=game.p1))

# turn 7 state
        self.assertEqual(game.p1.hp, 2)
        self.assertEqual(game.p1.armor, 7)
        self.assertEqual(game.p1.mana, 340)
        self.assertEqual(game.p2.hp, 12)
        self.assertEqual(game.effects, [{
            'name': 'Shield',
            'timer': 2,
            'caster': game.p1,
            'target': None
        }])
# turn 7
        game.apply_effects()
        self.assertTrue(game.cast(player=game.p1, spell='Poison', target=game.p2))

# turn 8 state
        self.assertEqual(game.p1.hp, 2)
        self.assertEqual(game.p1.armor, 7)
        self.assertEqual(game.p1.mana, 167)
        self.assertEqual(game.p2.hp, 12)
        self.assertEqual(game.effects, [{
            'name': 'Shield',
            'timer': 1,
            'caster': game.p1,
            'target': None
        }, {
            'name': 'Poison',
            'timer': 5,
            'caster': game.p1,
            'target': game.p2
        }])
# turn 8
        game.apply_effects()
        self.assertTrue(game.attack(player=game.p2, target=game.p1))

# turn 9 state
        self.assertEqual(game.p1.hp, 1)
        self.assertEqual(game.p1.armor, 7)
        self.assertEqual(game.p1.mana, 167)
        self.assertEqual(game.p2.hp, 9)
        self.assertEqual(game.effects, [{
            'name': 'Shield',
            'timer': 0,
            'caster': game.p1,
            'target': None
        }, {
            'name': 'Poison',
            'timer': 4,
            'caster': game.p1,
            'target': game.p2
        }])
# turn 9
        game.apply_effects()
        self.assertTrue(game.cast(player=game.p1, spell='Magic Missile', target=game.p2))

# turn 10 state
        self.assertEqual(game.p1.hp, 1)
        self.assertEqual(game.p1.armor, 7)
        self.assertEqual(game.p1.mana, 167)
        self.assertEqual(game.p2.hp, 2)
        self.assertEqual(game.effects, [{
            'name': 'Poison',
            'timer': 4,
            'caster': game.p1,
            'target': game.p2
        }])
# turn 10 
        game.apply_effects()
        self.assertFalse(game.attack(player=game.p2, target=game.p1))

if __name__ == '__main__':
    unittest.main()


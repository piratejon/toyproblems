#!/usr/bin/python3

import unittest

import lol

class P21TestCases(unittest.TestCase):
    def test_example_round(self):
        player = lol.Player(name='player', hp=8, dmg=5, ap=5)
        boss = lol.Player(name='boss', hp=12, dmg=7, ap=2)

        game = lol.Game(p1=player, p2=boss)

        self.assertFalse(game.has_winner())
        attack = game.attack(attacker=game.p1, target=game.p2)
        self.assertEqual(attack['dmg'], 3)
        self.assertEqual(game.p1.hp, 8)
        self.assertEqual(game.p2.hp, 9)

        self.assertFalse(game.has_winner())
        attack = game.attack(attacker=game.p2, target=game.p1)
        self.assertEqual(attack['dmg'], 2)
        self.assertEqual(game.p1.hp, 6)
        self.assertEqual(game.p2.hp, 9)

        self.assertFalse(game.has_winner())
        attack = game.attack(attacker=game.p1, target=game.p2)
        self.assertEqual(attack['dmg'], 3)
        self.assertEqual(game.p1.hp, 6)
        self.assertEqual(game.p2.hp, 6)

        self.assertFalse(game.has_winner())
        attack = game.attack(attacker=game.p2, target=game.p1)
        self.assertEqual(attack['dmg'], 2)
        self.assertEqual(game.p1.hp, 4)
        self.assertEqual(game.p2.hp, 6)

        self.assertFalse(game.has_winner())
        attack = game.attack(attacker=game.p1, target=game.p2)
        self.assertEqual(attack['dmg'], 3)
        self.assertEqual(game.p1.hp, 4)
        self.assertEqual(game.p2.hp, 3)

        self.assertFalse(game.has_winner())
        attack = game.attack(attacker=game.p2, target=game.p1)
        self.assertEqual(attack['dmg'], 2)
        self.assertEqual(game.p1.hp, 2)
        self.assertEqual(game.p2.hp, 3)

        self.assertFalse(game.has_winner())
        attack = game.attack(attacker=game.p1, target=game.p2)
        self.assertEqual(attack['dmg'], 3)
        self.assertEqual(game.p1.hp, 2)
        self.assertEqual(game.p2.hp, 0)

        self.assertTrue(game.has_winner())

if __name__ == '__main__':
    unittest.main()


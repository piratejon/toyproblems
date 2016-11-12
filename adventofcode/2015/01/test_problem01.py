#!/usr/bin/python3

import unittest

import problem01

class TestProblem01(unittest.TestCase):
    """test it and figure it out lol"""

    def test_examples(self):
        """exercise the examples provided in the problem"""
        self.assertEqual(problem01.problem01('(())'), 0)
        self.assertEqual(problem01.problem01('()()'), 0)
        self.assertEqual(problem01.problem01('((('), 3)
        self.assertEqual(problem01.problem01('(()(()('), 3)
        self.assertEqual(problem01.problem01('())'), -1)
        self.assertEqual(problem01.problem01('))('), -1)
        self.assertEqual(problem01.problem01(')))'), -3)
        self.assertEqual(problem01.problem01(')())())'), -3)

if __name__ == '__main__':
    unittest.main()


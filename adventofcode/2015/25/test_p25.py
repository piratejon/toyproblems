#!/usr/bin/python3

import unittest

import lol

class P25Test(unittest.TestCase):
    def test_codeiterator(self):
        cg = lol.CodeIterator(20151125)
        self.assertEqual(next(cg), 31916031)
        self.assertEqual(next(cg), 18749137)
        self.assertEqual(next(cg), 16080970)
        self.assertEqual(next(cg), 21629792)
        self.assertEqual(next(cg), 17289845)

    def test_cantoriterator(self):
        ci = lol.CantorIterator()
        self.assertEqual(next(ci), (2, 1, 31916031))
        self.assertEqual(next(ci), (1, 2, 18749137))
        self.assertEqual(next(ci), (3, 1, 16080970))
        self.assertEqual(next(ci), (2, 2, 21629792))
        self.assertEqual(next(ci), (1, 3, 17289845))
        self.assertEqual(next(ci), (4, 1, 17289845))
        self.assertEqual(next(ci), (1, 3, 17289845))
        self.assertEqual(next(ci), (1, 3, 17289845))
        self.assertEqual(next(ci), (1, 3, 17289845))
        self.assertEqual(next(ci), (1, 3, 17289845))
        self.assertEqual(next(ci), (1, 3, 17289845))
        self.assertEqual(next(ci), (1, 3, 17289845))
        self.assertEqual(next(ci), (1, 3, 17289845))

if __name__ == '__main__':
    unittest.main()


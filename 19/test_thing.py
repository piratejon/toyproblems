#!/usr/bin/python3

import unittest

import p19

class TestP19(unittest.TestCase):

    def test_rudolph(self):
        p = p19.P19()
        p.add_replacement('H', 'HO')
        p.add_replacement('H', 'OH')
        p.add_replacement('O', 'HH')
        p.do_it_to_it('HOH')
        self.assertEqual(p.molecules, {'HOOH', 'HOHO', 'OHOH', 'HHHH'})
        p.do_it_to_it('HOHOHO')
        self.assertEqual(len(p.molecules), 7)

if __name__ == '__main__':
    unittest.main()


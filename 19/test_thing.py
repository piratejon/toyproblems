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

    def test_rednose(self):
        p = p19.P19()
        p.add_replacement('e', 'H')
        p.add_replacement('e', 'O')
        p.add_replacement('H', 'HO')
        p.add_replacement('H', 'OH')
        p.add_replacement('O', 'HH')

        self.assertEqual(p.pieces('HOH', 0), {1: {'e'}, 2: {'H'}})
        self.assertEqual(p.pieces('HOH', 1), {1: {'e'}, 2: {'H'}})
        self.assertEqual(p.pieces('HOH', 2), {1: {'e'}})

    def test_ruffian(self):
        p = p19.P19()
        instr = ''
        mode = 'rules'
        with open('input', 'r') as fin:
            for line in fin.readlines():
                if mode == 'rules' and len(line.strip()) > 0:
                    words = line.split()
                    p.add_replacement(words[0], words[2])
                else:
                    mode = 'input'
                    instr = line.strip()

        self.assertEqual(len(instr), 468)

        self.assertEqual(p.pieces(instr, 3), {4: {'Si'}})
        self.assertEqual(p.pieces('BFFBF', 3), {2: {'Mg'}})

if __name__ == '__main__':
    unittest.main()


#!/usr/bin/env

'''solve aoc 2017 day 10'''

import functools
import operator
import sys

class Knot:
    '''implement a hashable knot'''

    def __init__(self, length=5):
        '''set us up an untwisted rope'''
        self.list = [_ for _ in range(length)]
        self.len = len(self.list)
        self.curpos = 0
        self.skip = 0

    def __getitem__(self, idx):
        '''index modulo len(list)'''
        print(idx)
        if type(idx).__name__ == 'slice':
            start, stop = idx.start, idx.stop
            return [self.list[_ % self.len] for _ in range(start, stop)]
        elif isinstance(idx, int):
            return self.list[idx % self.len]

    def __setitem__(self, idx, val):
        '''allow setting with idx modulo len(list)'''
        print('set', idx, val)
        if type(idx).__name__ == 'slice':
            start, stop = idx.start, idx.stop
            for _ in range(start, stop):
                self[_] = val[_ - start]
        elif type(idx).__name__ == 'int':
            self.list[idx % self.len] = val

    def do_it(self, length):
        '''hash iter given this length'''
        if length < 0 or length > self.len:
            raise ValueError(length)
        print('List:', self.list, 'curpos:', self.curpos, 'skiplen:', self.skip, 'Length:', length)
        self.reverse(length)

        self.curpos += length + self.skip
        self.skip += 1

    def reverse(self, length):
        '''reverse length items starting at curpos+skip'''
        print('reverse', length, self.curpos, self.curpos + length)
        revme = self[self.curpos:self.curpos + length]
        reved = [_ for _ in reversed(revme)]
        print('reverse', length, len(revme), revme, reved)
        self[self.curpos:self.curpos + length] = reved

    def finalize(self):
        '''sparse hash, dense hash, etc'''

        return [
            functools.reduce(operator.xor, [self[_] for _ in range(i, i + 16)])
            for i in range(0, 256, 16)
        ]

def main(filename):
    '''driver'''
    if filename:
        with open(filename, 'r') as fin:
            ropelen, llist = 256, [int(_) for _ in fin.read().split(',')]
    else:
        ropelen, llist = 5, [3, 4, 1, 5]

    knot = Knot(ropelen)

    for _ in llist:
        knot.do_it(_)
        print()
    print(knot.list)
    print(knot.list[0] * knot.list[1])

    if filename:
        with open(filename, 'r') as fin:
            ropelen, llist = 256, [ord(_) for _ in fin.read().strip()]
    else:
        ropelen, llist = 256, [ord(_) for _ in '']

    llist += [17, 31, 73, 47, 23]

    newknot = Knot(ropelen)
    for i in range(64):
        for _ in llist:
            newknot.do_it(_)

    print(''.join('{:02x}'.format(_) for _ in newknot.finalize()))

if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) == 2 else None)

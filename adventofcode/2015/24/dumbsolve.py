#!/usr/bin/env python3

'''
try to solve AoC 2015 day 22 in TYOOL 2017
'''

import functools
import operator
import sys

def part1_bruteforce(filename, group_size=3):
    with open(filename, 'r') as fin:
        W = [int(_) for _ in fin]
    _sum = sum(W)
    _group_weight = int(sum(W) / group_size)
    print(W, _sum, _group_weight)

    found = set()

    def keep_trying(unused_items, used_items, running_weight=0):
        if running_weight == _group_weight:
            found.add(tuple(sorted(used_items)))
        else:
            for item in sorted(unused_items, reverse=True):
                if running_weight + item <= _group_weight:
                    keep_trying(
                        unused_items - set((item,)),
                        used_items.union(set((item,))),
                        running_weight + item
                    )

    keep_trying(set(W), set())

    for _ in sorted(found, key=lambda x: (len(x), functools.reduce(operator.mul, x, 1))):
        print(_, functools.reduce(operator.mul, _, 1))

def main(args):
    '''driver'''
    #part1(args[0])
    part1_bruteforce(args[0])

if __name__ == '__main__':
    main(sys.argv[1:] or ['test.input'])

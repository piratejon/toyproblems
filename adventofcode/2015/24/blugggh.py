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

    def keep_digging(target, unused, used=[]):
        if len(used) > 7:
            return
        if target == 0:
            yield used
        else:
            for i in range(len(unused)):
                if unused[i] <= target:
                    yield from keep_digging(target - unused[i], unused[i+1:], used + [unused[i]])

    for _ in sorted(keep_digging(_group_weight, sorted(W, reverse=True)), key=lambda x: (len(x), functools.reduce(operator.mul, x, 1))):
        print(_, functools.reduce(operator.mul, _, 1))

def main(args):
    '''driver'''
    part1_bruteforce(args[0])

if __name__ == '__main__':
    main(sys.argv[1:] or ['test.input'])

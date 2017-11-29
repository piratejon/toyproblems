#!/usr/bin/env python3

'''
try to solve AoC 2015 day 22 in TYOOL 2017
'''

from functools import reduce
from operator import itemgetter, mul
import itertools
import unittest
import sys
import z3

def part1(filename):
    '''pick up the file make some z3s etc'''
    with open(filename, 'r') as fin:
        W = [int(_) for _ in fin]
    _sum = sum(W)
    _group_weight = sum(W) / 3
    print(W, _sum, _group_weight)

    # make variables not war
    G = [[z3.Int('w_{}_{}'.format(j,i)) for i in range(len(W))] for j in range(3)]

    # each column must have exactly one row populated with a true weight
    col_c = [
        z3.Or(
            z3.And(G[0][i] == W[i], G[1][i] == 0, G[2][i] == 0),
            z3.And(G[0][i] == 0, G[1][i] == W[i], G[2][i] == 0),
            z3.And(G[0][i] == 0, G[1][i] == 0, G[2][i] == W[i])
        )
        for i in range(len(W))
    ]
    #print(col_c)

    # all row sums must be equal
    row_c = [
        z3.And(
            z3.Sum([G[0][i] for i in range(len(W))]) == z3.Sum([G[1][i] for i in range(len(W))]),
            z3.Sum([G[0][i] for i in range(len(W))]) == z3.Sum([G[2][i] for i in range(len(W))])
        )
    ]
    #print(row_c)

    # row0 must have exactly 3 nonzeroes
    # this should eliminate the (9, 11) group and leave 9 3-tuples
    target_non_zeroes = 5
    row0_c = [
        z3.Or([
            z3.And([_ == 0 for _ in col])
            for col in itertools.combinations(group, len(group) - target_non_zeroes)
        ])
        for group in [G[0]]
    ]
    print(row0_c)

    #### bLARRGH
    #fake_c = [G[0][7] == 0, G[0][9] == 0]
    fake_c = [z3.Or(G[row][7] == 0, G[row][9] == 0) for row in range(3)]
    print(fake_c)

    s = z3.Solver()
    s.add(col_c + row_c + row0_c)
    models = {}
    while s.check() == z3.sat:
        m = s.model()
        g = [[m.evaluate(G[row][col]) for col in range(len(W))] for row in range(3)]
        h = [[g[row][col].as_long() for col in range(len(W)) if g[row][col].as_long() != 0] for row in range(3)]
        #print(h)
        tup = tuple(sorted([(len(_), reduce(mul, _, 1), tuple(_)) for _ in h], key=itemgetter(0)))
        print(tup)
        if tup not in models:
            models[tup] = 0
        models[tup] += 1

        s.add(z3.Or([d() != m[d] for d in m]))

    else:
        print('unsat')

    print('unique models:', len(models))
    print('total models:', sum(v for k, v in models.items()))
    if models:
        uniques = set([m[0] for m in models.keys()])
        print(len(uniques))
        print(sorted(uniques, key=itemgetter(0)))
        print()
        print(sorted(uniques, key=itemgetter(1)))

def part1_bruteforce(filename, group_size=3):
    with open(filename, 'r') as fin:
        W = [int(_) for _ in fin]
    _sum = sum(W)
    _group_weight = sum(W) / 3
    print(W, _sum, _group_weight)

    def keep_trying(unused_items, used_items=set(), running_weight=0):
        if running_weight == _group_weight:
            yield used_items
        else:
            for item in sorted(unused_items, reverse=True):
                if running_weight + item <= _group_weight:
                    keep_trying(unused_items - set((item,)), used_items.union(set((item,))), running_weight + item)

    for _ in keep_trying(set(W)):
        print(_)

def main(args):
    '''driver'''
    #part1(args[0])
    part1_bruteforce(args[0])

if __name__ == '__main__':
    main(sys.argv[1:] or ['test.input'])

#!/usr/bin/env pypy3

import sys

with open('test.input' if len(sys.argv) != 2 else sys.argv[1], 'r') as fin:
    items = [tuple(int(_) for _ in line.split(':')) for line in fin]

    hits = []
    mul = 0
    for l, r in items:
        if l % ((r - 1) * 2) == 0:
            hits.append(l)
            mul += (l * r)
    print('part1', mul, hits)

    wait = 0
    while True:
        if any((wait + _[0]) % ((_[1] - 1) * 2) == 0 for _ in items):
            wait += 1
        else:
            break
    print('part2', wait)

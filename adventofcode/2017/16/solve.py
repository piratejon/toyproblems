#!/usr/bin/env python3

import sys

def spin(fwd, rev, n, size):
    fwd = {k: (v + n) % size for k, v in fwd.items()}
    rev = {v: k for k, v in fwd.items()}
    return fwd, rev

def partner(fwd, rev, cmd):
    a, b = cmd.split('/')
    fwd[a], fwd[b] = int(fwd[b]), int(fwd[a])
    rev = {v: k for k, v in fwd.items()}
    return fwd, rev

def exchange(fwd, rev, cmd):
    a, b = tuple(int(_) for _ in cmd.split('/'))
    rev[a], rev[b] = rev[b], rev[a]
    fwd = {k: v for v, k in rev.items()}
    return fwd, rev

def dance(fwd, rev, insns):
    for insn in insns:
        if insn[0] == 's':
            fwd, rev = spin(fwd, rev, int(insn[1:]), len(fwd))
        elif insn[0] == 'x':
            fwd, rev = exchange(fwd, rev, insn[1:])
        elif insn[0] == 'p':
            fwd, rev = partner(fwd, rev, insn[1:])
        else:
            raise ValueError('Unknown instruction', insn)
    return fwd, rev

def setup_dict(string):
    fwd = {_: i for i, _ in enumerate(string)}
    rev = {v: k for k, v in fwd.items()}
    return fwd, rev

def restring(rev):
    return ''.join(rev[_] for _ in sorted(rev.keys()))

with open('test.input' if len(sys.argv) != 2 else sys.argv[1], 'r') as fin:
    insns = [_.strip() for _ in fin.read().split(',') if len(_.strip()) > 0]

    src = ''.join(chr(ord('a') + i) for i in range(16))
    fwd, rev = setup_dict(src)
    fwd, rev = dance(fwd, rev, insns)
    print('part1', restring(rev))

    fwd, rev = setup_dict(src)
    found = set(restring(rev))
    for i in range(1000000000):
        rstr = restring(rev)
        print(i, rstr)
        if rstr not in found:
            found.add(rstr)
        else:
            print(rstr, 'found after', i, 'iterations')
            break
        fwd, rev = dance(fwd, rev, insns)

    for i in range(1000000000 % i):
        fwd, rev = dance(fwd, rev, insns)
    print('part2', restring(rev))

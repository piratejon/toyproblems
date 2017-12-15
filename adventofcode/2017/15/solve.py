#!/usr/bin/env python3

a, b = 65, 8921
a, b = 703, 516
a_fac, b_fac = 16807, 48271
div = 2147483647

_a, _b = a, b
matches = 0
for i in range(40000000):
    _a = (_a * a_fac) % div
    _b = (_b * b_fac) % div
    match = 1 if (_a ^ _b) & 0xffff == 0 else 0
    #print(_a, _b, match)
    matches += match
print('part1', matches)

_a, _b = a, b
matches = 0
for i in range(5000000):

    _a = (_a * a_fac) % div
    while _a % 4 != 0:
        _a = (_a * a_fac) % div

    _b = (_b * b_fac) % div
    while _b % 8 != 0:
        _b = (_b * b_fac) % div

    match = 1 if (_a ^ _b) & 0xffff == 0 else 0
    matches += match
print('part2', matches, _a, _b)

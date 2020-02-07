#!/usr/bin/python3

def part1():
    _sum = 0
    with open('input', 'r') as fin:
        for line in fin:
            line = int(line)
            _sum += (line // 3) - 2
    print(_sum)

def do_one_line(x):
    _sum = x
    _sums = []
    while _sum > 0:
        _sum = (_sum // 3) - 2
        if _sum > 0:
            _sums += [_sum]
        else:
            break
    return sum(_sums)


def part2():
    if False:
        print(_sums, sum(_sums))
    if True:
        with open('input', 'r') as fin:
            print(sum([do_one_line(int(_)) for _ in fin.readlines()]))

part2()

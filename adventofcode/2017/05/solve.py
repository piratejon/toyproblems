#!/usr/bin/env python3

import sys

def do_the_jumps_part1(lines):
    start = 0
    stop = len(lines)
    c = 0
    i = start
    while i >= start and i < stop:
        jmprel = lines[i]
        lines[i] += 1
        i += jmprel
        c += 1
        #print(lines, c, i)
    print(c)

def do_the_jumps_part2(lines):
    start = 0
    stop = len(lines)
    c = 0
    i = start
    while i >= start and i < stop:
        jmprel = lines[i]
        if jmprel >= 3:
            lines[i] -= 1
        else:
            lines[i] += 1
        i += jmprel
        c += 1
        #print(lines, c, i)
    print(c)

def main(filename):
    with open(filename, 'r') as fin:
        lines = [int(_) for _ in fin]
        #do_the_jumps_part1(lines)
        do_the_jumps_part2(lines)

if __name__ == '__main__':
    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        main('testinput')

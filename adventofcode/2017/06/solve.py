#!/usr/bin/env python3

def cycle1(blocks):
    # find first max
    m = max(blocks)
    b = len(blocks)
    for i in range(len(blocks)):
        if blocks[i] == m:
            blocks[i] = 0
            #print('found {} at {}'.format(m, i))
            break
    # now i has the max index
    j = i
    while m > 0:
        i += 1
        #print('adding 1 to {}'.format(i % b))
        blocks[i % b] += 1
        m -= 1

    #print('fin')

    return blocks

def part1(data):
    seen = set()

    c = 0
    while tuple(data) not in seen:
        #print(data)
        seen.add(tuple(data))
        data = cycle1(data)
        c += 1

    print(data)
    print(seen)

    print(c)

    return data

def part2(data):
    c = 0
    print(data)
    d0 = tuple(data)

    while True:
        c += 1
        data = cycle1(data)
        if tuple(data) == d0:
            break
    print(c)

with open('input', 'r') as fin:
    data = [int(_) for _ in next(fin).split()]
    #data = [0, 2, 7, 0]
    data = part1(data)
    part2(data)

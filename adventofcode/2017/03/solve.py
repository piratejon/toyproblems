#!/usr/bin/env python3

'''solve AoC 2017 Day 3'''

INPUT = 312051

GRID = [[0 for _ in range(1000)] for _ in range(1000)]
CENTER_OFFSET = 500

def write_grid(x, y, i):
    '''put a new value in the grid'''
    GRID[x + CENTER_OFFSET][y + CENTER_OFFSET] = i

def read_grid(x, y):
    '''fetch a value from the grid'''
    return GRID[x + CENTER_OFFSET][y + CENTER_OFFSET]

def update_grid(X, Y):
    '''do the grid updates for part 2'''
    c = 0
    for x in range(X - 1, X + 2):
        for y in range(Y - 1, Y + 2):
            c += read_grid(x, y)

    print('total', X, Y, c)
    write_grid(X, Y, c)
    if c > INPUT:
        print('c > p', c)
        raise ValueError()

def find_coords(p):
    '''solve part 1'''
    x, y = 0, 0
    c = 1
    i = 1
    write_grid(x, y, i) # initial [0, 0]
    while i < p:
        # right
        for _ in range(c):
            i += 1
            x += 1
            update_grid(x, y)
            if i == p:
                return x, y

        # up
        for _ in range(c):
            i += 1
            y += 1
            update_grid(x, y)
            if i == p:
                return x, y

        c += 1

        # left
        for _ in range(c):
            i += 1
            x -= 1
            update_grid(x, y)
            if i == p:
                return x, y

        # down
        for _ in range(c):
            i += 1
            y -= 1
            update_grid(x, y)
            if i == p:
                return x, y

        c += 1

coords = find_coords(INPUT)
print(coords, abs(coords[0]) + abs(coords[1]))

#!/usr/bin/python3

import sys

rx, ry = 0, 0
sx, sy = rx, ry

grid = {(0,0): 1}

def move(x, y, where):
    if where == '^':
        return (x,y+1)
    elif where == '>':
        return (x+1,y)
    elif where == 'v':
        return (x,y-1)
    elif where == '<':
        return (x-1,y)
    else:
        raise ValueError('unexpected', where, 'at', x, y)

turn = 0
for char in sys.stdin.read():

    if turn % 2 == 0:
        sx, sy = move(sx, sy, char)
        if (sx, sy) not in grid.keys():
            grid[(sx, sy)] = 0

    else:
        rx, ry = move(rx, ry, char)
        if (rx, ry) not in grid.keys():
            grid[(rx, ry)] = 0

    turn += 1

print(len(grid.keys()))


#!/usr/bin/python env

'''solve AoC 2017 Day 11'''

import math
import sys

class HexGrid:
    '''infinite hex grid, no problem'''

    def __init__(self):
        '''start us in the middle'''
        self.x = self.y = self.max = 0

    def reset(self):
        '''go back'''
        self.x = self.y = self.max = 0

    def move(self, where):
        '''go somewhere'''
        if where == 'n':
            self.y += 1
        elif where == 'ne':
            self.x += 1
        elif where == 'se':
            self.x += 1
            self.y -= 1
        elif where == 's':
            self.y -= 1
        elif where == 'sw':
            self.x -= 1
        elif where == 'nw':
            self.x -= 1
            self.y += 1
        else:
            raise ValueError(where)

        self.max = max(self.max, self.distance())

    def distance(self):
        '''
        how far are we
        <http://www.drking.org.uk/hexagons/misc/grid.html> 2017-12-11
        <http://www-cs-students.stanford.edu/~amitp/Articles/HexLOS.html> 2017-12-12
        '''
        if math.copysign(1, self.x) == math.copysign(1, self.y):
            return abs(self.x) + abs(self.y)
        else:
            return max(abs(self.x), abs(self.y))

g = HexGrid()
for _ in 'ne,ne,ne'.split(','):
    g.move(_)
print(g.x, g.y, g.distance())
g.reset()
for _ in 'ne,ne,sw,sw'.split(','):
    g.move(_)
print(g.x, g.y, g.distance())
g.reset()
for _ in 'ne,ne,s,s'.split(','):
    g.move(_)
print(g.x, g.y, g.distance())
g.reset()
for _ in 'se,sw,se,sw,sw'.split(','):
    g.move(_)
print(g.x, g.y, g.distance())
g.reset()
with open(sys.argv[1], 'r') as fin:
    for _ in fin.read().strip().split(','):
        g.move(_)
    print(g.x, g.y, g.distance(), g.max)

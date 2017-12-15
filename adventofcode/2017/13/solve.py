#!/usr/bin/env pypy3

import sys

class Empty:

    def __init__(self):
        self.pos = -1

    def tick(self):
        pass

    def __repr__(self):
        return 'e'

class Layer:

    def __init__(self, range_):
        self.range = range_
        self.max = self.range - 1
        self.pos = 0
        self.dir = 1

    def tick(self):
        if self.dir == 1:
            self.pos += 1
            if self.pos == self.max:
                self.dir = 0
        else:
            self.pos -= 1
            if self.pos == 0:
                self.dir = 1

    def __repr__(self):
        return '{}/{}{}'.format(self.pos, self.max, '+' if self.dir == 1 else '-')

dlayers = {}
layers = []
hits = []
with open('test.input' if len(sys.argv) != 2 else sys.argv[1], 'r') as fin:
    for line in fin:
        x = line.split(':')
        dlayers[int(x[0])] = Layer(int(x[1]))

    m = max(dlayers.keys())
    for i in range(m + 1):
        if i in dlayers:
            layers.append(dlayers[i])
        else:
            layers.append(Empty())

def tick_all(l):
    for _ in layers:
        _.tick()

for pos in range(m + 1):
    print(pos, layers)
    if layers[pos].pos == 0:
        hits.append(pos)
    tick_all(layers)

mul = 0
for _ in hits:
    mul += (_ * layers[_].range)
print(mul)


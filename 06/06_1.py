#!/usr/bin/python3

import fileinput

lights = set()

class IterRange:
    def __init__(self, corner0, corner1):
        c0 = corner0.split(',')
        c1 = corner1.split(',')
        self.x_range = sorted([int(_) for _ in [c0[0], c1[0]]])
        self.y_range = sorted([int(_) for _ in [c0[1], c1[1]]])
        self.x = self.x_range[0]
        self.y = self.y_range[0]
        self.i = self.x + (self.y * 1000)
#print('iter', self.x_range, self.y_range)

    def __iter__(self):
        self.x = self.x_range[0] - 1
        self.y = self.y_range[0]

        return self

    def __next__(self):
        if self.x >= self.x_range[1]:
            self.x = self.x_range[0]
            if self.y >= self.y_range[1]:
                raise StopIteration
            else:
                self.y += 1
        else:
            self.x += 1
        self.i = self.x + (self.y * 1000)
        return self.i

def turn_on(start, fin):
    for i in IterRange(start, fin):
        lights.add(i)

def turn_off(start, fin):
    for i in IterRange(start, fin):
        lights.discard(i)

def toggle(start, fin):
    for i in IterRange(start, fin):
        try:
            lights.remove(i)
        except KeyError:
            lights.add(i)

for line in fileinput.input():
    words = line.split()
    if words[0] == 'turn':
        if words[1] == 'on':
            turn_on(words[2], words[4])
        elif words[1] == 'off':
            turn_off(words[2], words[4])
        else:
            raise ValueError(line)
    elif words[0] == 'toggle':
        toggle(words[1], words[3])
    else:
        raise ValueError(line)

print(len(lights))


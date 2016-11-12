#!/usr/bin/python3

import fileinput

sqft = 0
ribbon = 0
for line in fileinput.input():
    l, w, h = [int(x) for x in line.split('x')]
    sides = [l*w, w*h, h*l]
    sqft += sum([2*s for s in sides])
    sqft += min(sides)

    ribbon += (l*w*h)
    ribbon += sum([2*a for a in sorted([l,w,h])[:2]])
print(sqft, ribbon)


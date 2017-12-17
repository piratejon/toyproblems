#!/usr/bin/env python3

import sys

jump = 349

state = [0]
pos = 0

for insert_me in range(1, 2018):
    pos += jump
    pos %= len(state)
    state = state[:pos + 1] + [insert_me] + state[pos + 1:]
    pos += 1

for _ in range(len(state)):
    if state[_] == 2017:
        print(state[_ + 1])
        break

state = [0]
pos = 0
after_zero = -1

for insert_me in range(1, 50000001):
    pos += jump
    pos %= (insert_me)
    if pos == 0:
        after_zero = insert_me
    #state = state[:pos + 1] + [insert_me] + state[pos + 1:]
    pos += 1

print(after_zero)


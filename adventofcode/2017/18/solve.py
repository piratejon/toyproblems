#!/usr/bin/env python3

import sys

with open('test.input' if len(sys.argv) != 2 else sys.argv[1], 'r') as fin:
    reg = {chr(ord('a') + i): 0 for i in range(26)}
    insns = [_.split() for _ in fin]
    sounds = []

    pc = 0
    while pc >= 0 and pc < len(insns):
        i = insns[pc]
        print(pc, i)
        if i[0] == 'set':
            try:
                reg[i[1]] = int(i[2])
            except ValueError:
                reg[i[1]] = reg[i[2]]
            pc += 1
        elif i[0] == 'mul':
            try:
                reg[i[1]] *= int(i[2])
            except ValueError:
                reg[i[1]] *= reg[i[2]]
            pc += 1
        elif i[0] == 'jgz':
            pc += int(i[2]) if reg[i[1]] > 0 else 1
        elif i[0] == 'snd':
            try:
                sounds.append(int(i[1]))
            except ValueError:
                sounds.append(reg[i[1]])
            pc += 1
        elif i[0] == 'add':
            reg[i[1]] += int(i[2])
            pc += 1
        elif i[0] == 'mod':
            try:
                reg[i[1]] %= int(i[2])
            except ValueError:
                reg[i[1]] %= reg[i[2]]
            pc += 1
        elif i[0] == 'rcv':
            print('recover {}'.format(sounds[-1]) if reg[i[1]] != 0 else 'no recovery')
            if reg[i[1]] != 0:
                break
            pc += 1

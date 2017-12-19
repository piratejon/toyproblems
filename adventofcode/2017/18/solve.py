#!/usr/bin/env python3

import sys

class Program:
    '''encapsulate the program so we can run two at once'''

    def __init__(self, insns):
        self.insns = [_ for _ in insns]
        self.reg = {chr(ord('a') + i): 0 for i in range(26)}
        self.pc = 0
        self.sounds = []

    def execute(self):
        while self.pc >= 0 and self.pc < len(self.insns):
            i = self.insns[self.pc]
            print(self.pc, i)
            if i[0] == 'set':
                try:
                    self.reg[i[1]] = int(i[2])
                except ValueError:
                    self.reg[i[1]] = self.reg[i[2]]
                self.pc += 1
            elif i[0] == 'mul':
                try:
                    self.reg[i[1]] *= int(i[2])
                except ValueError:
                    self.reg[i[1]] *= self.reg[i[2]]
                self.pc += 1
            elif i[0] == 'jgz':
                self.pc += int(i[2]) if self.reg[i[1]] > 0 else 1
            elif i[0] == 'snd':
                try:
                    self.sounds.append(int(i[1]))
                except ValueError:
                    self.sounds.append(self.reg[i[1]])
                self.pc += 1
            elif i[0] == 'add':
                self.reg[i[1]] += int(i[2])
                self.pc += 1
            elif i[0] == 'mod':
                try:
                    self.reg[i[1]] %= int(i[2])
                except ValueError:
                    self.reg[i[1]] %= self.reg[i[2]]
                self.pc += 1
            elif i[0] == 'rcv':
                print('recover {}'.format(self.sounds[-1]) if self.reg[i[1]] != 0 else 'no recovery')
                if self.reg[i[1]] != 0:
                    break
                self.pc += 1

with open('test.input' if len(sys.argv) != 2 else sys.argv[1], 'r') as fin:
    insns = [_.split() for _ in fin]

    a = Program(insns)
    a.execute()

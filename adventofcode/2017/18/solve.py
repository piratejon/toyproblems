#!/usr/bin/env python3

import sys

class Program:
    '''encapsulate the program so we can run two at once'''

    def __init__(self, insns, pid):
        self.insns = [_ for _ in insns]
        self.reg = {chr(ord('a') + i): 0 for i in range(26)}
        self.pc = 0
        self.sounds = []
        self.pid = pid
        self.reg['p'] = pid

    def tick(self, arb):
        if self.pc < 0 or self.pc >= len(self.insns):
            raise IndexError('PC {} out of bounds [0, {})'.format(self.pc, len(self.insns)))

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
                arb.send(int(i[1]))
            except ValueError:
                arb.send(self.reg[i[1]])
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
            self.reg[i[1]] = arb.recv(self.reg[i[1]])
            self.pc += 1

class ArbPart1:
    '''send messages back and forth between two programs'''

    def __init__(self, insns):
        a = Program(insns, 0)
        self.value = None

        while True:
            try:
                a.tick(self)
            except ValueError:
                break

        print(self.value)

    def send(self, value):
        self.value = value

    def recv(self, value):
        if value != 0:
            raise ValueError
        else:
            return value

with open('test.input' if len(sys.argv) != 2 else sys.argv[1], 'r') as fin:
    insns = [_.split() for _ in fin]

    # part1
    ArbPart1(insns)

    # part2
    #arb = Arbiter(insns)

#!/usr/bin/env python3

import sys

class Program:
    '''encapsulate the program so we can run two at once'''

    def __repr__(self):
        return 'Program {}: {}, {}:{}'.format(self.pid, self.status, self.pc, self.insns[self.pc])

    def __init__(self, insns, pid=0):
        self.pid = pid
        self.insns = [_ for _ in insns]
        self.reg = {chr(ord('a') + i): 0 for i in range(26)}
        self.pc = 0
        self.sounds = []
        self.pid = pid
        self.reg['p'] = pid
        self.status = 'running'

    def tick(self, arb):
        if self.pc < 0 or self.pc >= len(self.insns):
            self.running = False

        i = self.insns[self.pc]
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
            try:
                self.pc += int(i[2]) if self.reg[i[1]] > 0 else 1
            except ValueError:
                self.pc += self.reg[i[2]] if self.reg[i[1]] > 0 else 1
            except KeyError:
                self.pc += int(i[2]) if int(i[1]) > 0 else 1
        elif i[0] == 'snd':
            try:
                arb.send(self, int(i[1]))
            except ValueError:
                arb.send(self, self.reg[i[1]])
            self.pc += 1
        elif i[0] == 'add':
            try:
                self.reg[i[1]] += int(i[2])
            except ValueError:
                self.reg[i[1]] += self.reg[i[2]]
            self.pc += 1
        elif i[0] == 'mod':
            try:
                self.reg[i[1]] %= int(i[2])
            except ValueError:
                self.reg[i[1]] %= self.reg[i[2]]
            self.pc += 1
        elif i[0] == 'rcv':
            self.reg[i[1]] = arb.recv(self, self.reg[i[1]])
            self.pc += 1

class ArbPart1:
    '''wrapper for part1 to use the same framework as part2'''

    def __init__(self, insns):
        a = Program(insns)
        self.value = None

        while a.running:
            print(a, a.pc, a.insns[a.pc])
            a.tick(self)

        print('part1', self.value)

    def send(self, caller, value):
        self.value = value

    def recv(self, caller, value):
        if value != 0:
            caller.running = False
        else:
            return value

class ArbPart2:
    '''two programs duet, sending and receiving amongst each other'''

    def __init__(self, insns):
        self.a, self.b = Program(insns, 0), Program(insns, 1)
        self.other = {
            self.a: self.b,
            self.b: self.a
        }
        self.queue = {
            self.a: [],
            self.b: []
        }
        self.waiting = None

        print(set(self.other.keys()))

        self.a.status = 'running' if self.a.insns[0][0] != 'recv' else 'blocked'
        self.b.status = 'running' if self.b.insns[0][0] != 'recv' else 'blocked'

        prog = self.a
        while self.a.status == 'running' or self.b.status == 'running':
            print(prog)
            if prog.status == 'running':
                if self.queue[self.other[prog]] or prog.insns[prog.pc][0] != 'rcv':
                    prog.tick(self)
                else:
                    prog.status == 'blocked'

            prog = self.other[prog]

    def send(self, caller, value):
        self.queue[caller].append(value)
        print(caller, 'send', value, 'queue', self.queue[caller])

    def recv(self, caller, value):
        x, self.queue[self.other[caller]] = self.queue[self.other[caller]][0], self.queue[self.other[caller]][1:]
        print(caller, 'recv', x, 'queue', self.queue[self.other[caller]])
        return x

with open('test.input' if len(sys.argv) != 2 else sys.argv[1], 'r') as fin:
    insns = [_.split() for _ in fin]

    # part1
    try:
        ArbPart1(insns)
    except:
        pass

    # part2
    arb = ArbPart2(insns)

#!/usr/bin/env python3

import sys

def instruction_from_string(line):
  s = line.split()
  s[2] = int(s[2])
  s[6] = int(s[6])
  return s

def execute(insn, regs, highest):
  reg, op, operand, _, condreg, cond, val = insn

  if condreg not in regs:
    regs[condreg] = 0

  if reg not in regs:
    regs[reg] = 0

  if {
    '>': lambda c, v: c > v,
    '<': lambda c, v: c < v,
    '>=': lambda c, v: c >= v,
    '<=': lambda c, v: c <= v,
    '==': lambda c, v: c == v,
    '!=': lambda c, v: c != v,
  }[cond](regs[condreg], val):
    regs[reg] += operand if op == 'inc' else -operand

  highest = max(set(regs.values()) | {highest})

  return regs, highest

with open('test.input' if len(sys.argv) < 2 else sys.argv[1], 'r') as fin:
  insns = []
  regs = {}
  for line in fin:
    insns.append(instruction_from_string(line))

  highest = 0
  for insn in insns:
    regs, highest = execute(insn, regs, highest)
  print(regs)
  print(max(regs.values()))
  print(highest)

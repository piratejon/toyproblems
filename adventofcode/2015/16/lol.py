#!/usr/bin/python3

def parse_sue_line(line):
    w = line.split()
    if len(w) == 8:
        return {w[1][:-1]: {
            w[2][:-1]: int(w[3][:-1]),
            w[4][:-1]: int(w[5][:-1]),
            w[6][:-1]: int(w[7].strip()),
            }}
    else:
        return {}

def parse_param_line(line):
    w = line.split()
    if len(w) == 2:
        return {
            w[0][:-1]: int(w[1].strip())
        }

sues = {}
lol = {}

with open('input', 'r') as fin:
    for line in fin.readlines():
        sues.update(parse_sue_line(line))

with open('params', 'r') as fin:
    for line in fin.readlines():
        lol.update(parse_param_line(line))

def find_part1_aunt():
    winning_aunts = set(sues.keys())
    for auntid in sues:
        for k in lol:
            if k in sues[auntid] and sues[auntid][k] != lol[k]:
                winning_aunts.remove(auntid)
                break

    print(winning_aunts)

find_part1_aunt()

def find_part2_aunt():
    winning_aunts = set(sues.keys())
    for auntid in sues:
        for k in lol:
            if k in sues[auntid]:
                if k in {'cats', 'trees'}:
                    if sues[auntid][k] <= lol[k]:
                        winning_aunts.remove(auntid)
                        break
                elif k in {'pomeranians', 'goldfish'}:
                    if sues[auntid][k] >= lol[k]:
                        winning_aunts.remove(auntid)
                        break
                else:
                    if sues[auntid][k] != lol[k]:
                        winning_aunts.remove(auntid)
                        break
    print(winning_aunts)

find_part2_aunt()


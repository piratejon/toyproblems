#!/usr/bin/python3

import fileinput

import re

class P19:

    def __init__(self):
        self.replacements = {}
        self.invreplacements = {}
        self.molecules = set()
        self.longest_left = 0
        self.longest_right = 0

    def add_replacement(self, left, right):
        if left not in self.replacements:
            self.replacements[left] = set()
        if right not in self.invreplacements:
            self.invreplacements[right] = set()
        self.longest_left = max(self.longest_left, len(left))
        self.longest_right = max(self.longest_right, len(right))
        self.replacements[left].add(right)
        self.invreplacements[right].add(left)

    def do_part1(self, what):
        self.molecules = set()
        for k in self.replacements:
            i = what.find(k)
            if i == -1:
                pass
                #print('did not find', k, 'in', what)
            else:
                while i > -1:
                    for v in self.replacements[k]:
#print('found', k, v, 'in', what, 'at', i)
                        adding = what[:i] + v + what[i + len(k):]
                        self.molecules.add(adding)
                    i = what.find(k, i + 1)

    def pieces(self, string, pos):
        """i didn't document this and not sure what it does now"""
        piecebag = {}
        for i in range(len(string) - pos + 1):
            if string[pos:pos+i] in self.invreplacements:
                if i not in piecebag:
                    piecebag[i] = set()
                piecebag[i] |= self.invreplacements[string[pos:pos+i]]

        return piecebag

    def left_replacements(self, string, pos):
        """returns the guys we could substitute into string at pos, of any length"""
        piecebag = {}
        for e in range(1, self.longest_left + 1):
            left = string[pos:pos + e]
            if left in self.replacements:
                if len(left) not in piecebag:
                    piecebag[len(left)] = set()
                piecebag[len(left)] |= self.replacements[left]
        return piecebag

    def right_replacements(self, string, pos):
        """returns guys we could back-substitute at pos, of any length"""
        piecebag = {}
        for e in range(1, self.longest_right + 1):
            right = string[pos:pos + e]
            if right in self.invreplacements:
                if len(right) not in piecebag:
                    piecebag[len(right)] = set()
                piecebag[len(right)] |= self.invreplacements[right]
        return piecebag

    def do_substitutions(self, string, target, visited, step=1):
        if string == target:
            print(step)
        if len(string) >= len(target):
            return

        for pos in range(len(string)):
            for replacement_length, replacements in self.left_replacements(string, pos).items():
                for replacement in replacements:
                    new_string = string[:pos] + replacement + string[pos + replacement_length:]
                    if new_string not in visited:
                        pass
# visited.add(new_string)
#                        print(new_string, step, len(visited), len(new_string))
                    self.do_substitutions(new_string, target, visited, step + 1)

    def do_part2_forward(self, target):
        visited = set()
        for starting_symbol in self.replacements['e']:
            self.do_substitutions(starting_symbol, target, visited)

    def do_part2_backward(self, string, visited=set(), steps=1):
        if string == 'e':
            print(steps)
            return

        for pos in range(len(string)):
            for replacement_length, replacements in self.right_replacements(string, pos).items():
                for replacement in replacements:
                    new_string = string[:pos] + replacement + string[pos + replacement_length:]
                    if new_string not in visited:
                        visited.add(new_string)
#print(new_string, pos, replacement_length, replacement)
                        self.do_part2_backward(new_string, visited, steps + 1)

def main():
    p = P19()
    lines = fileinput.input()
    for line in lines:
        if len(line.strip()) == 0:
            break
        else:
            w = line.strip().split()
#            print('adding replacement', w[0], w[2])
            p.add_replacement(w[0], w[2])
    thing = next(lines).strip()
#    print('starting point', thing)

#    p.do_part1(thing)

#    print('molecules:', len(p.molecules))
#    print(p.molecules)

    p.do_part2_backward(thing)

if __name__ == '__main__':
    main()


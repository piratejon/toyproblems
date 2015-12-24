#!/usr/bin/python3

import fileinput

class P19:

    def __init__(self):
        self.replacements = {}
        self.invreplacements = {}
        self.molecules = set()

    def add_replacement(self, left, right):
        if left not in self.replacements:
            self.replacements[left] = set()
        if right not in self.invreplacements:
            self.invreplacements[right] = set()
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
        self.piecebag = {}
        for i in range(len(string) - pos + 1):
            if string[pos:pos+i] in self.invreplacements:
                if i not in self.piecebag:
                    self.piecebag[i] = set()
                self.piecebag[i] |= self.invreplacements[string[pos:pos+i]]

        return self.piecebag

    def do_more_derivations(self, string, target, level):
#        print(level, len(string), 'trying to build from', string)
        if string in self.molecules:
            print('skipping already explored', string)
            return
        else:
            self.molecules.add(string)

        if string == target:
            print('built string on level', level)
        elif len(string) < len(target):
            for pos in range(len(string)):
#                print('trying to do a derivation at', pos)
                for pos_2 in range(pos + 1, len(string)):
                    if string[pos:pos_2] in self.replacements:
                        for repl in self.replacements[string[pos:pos_2]]:
#                            print('found replacement', repl, 'for', string[pos:pos_2])
                            self.do_more_derivations(string[:pos] + repl + string[pos_2:], target, level + 1)
        else:
            pass
#            print('abandoning for length', len(string), string)

    def do_part2(self, target):
        self.molecules = set()
        for starting_symbol in self.replacements['e']:
            self.do_more_derivations(starting_symbol, target, 1)

def main():
    p = P19()
    lines = fileinput.input()
    for line in lines:
        if len(line.strip()) == 0:
            break
        else:
            w = line.strip().split()
            print('adding replacement', w[0], w[2])
            p.add_replacement(w[0], w[2])
    thing = next(lines).strip()
    print('starting point', thing)

    p.do_part1(thing)

    print('molecules:', len(p.molecules))
#    print(p.molecules)

    p.do_part2(thing)

if __name__ == '__main__':
    main()


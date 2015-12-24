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

    def do_it_to_it(self, what):
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
        for i in range(len(string) - pos):
            if string[pos:pos+i] in self.invreplacements:
                if i not in self.piecebag:
                    self.piecebag[i] = set()
                self.piecebag[i] |= self.invreplacements[string[pos:pos+i]]

        return self.piecebag

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

    p.do_it_to_it(thing)

    print('molecules:', len(p.molecules))
    print(p.molecules)

if __name__ == '__main__':
    main()


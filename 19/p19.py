#!/usr/bin/python3

import fileinput

class P19:

    def __init__(self):
        self.replacements = {}
        self.molecules = set()

    def add_replacement(self, left, right):
        if left not in self.replacements:
            self.replacements[left] = set()
        self.replacements[left].add(right)

    def do_it_to_it(self, what):
        orig_what = what
        self.molecules = set()
        for k in self.replacements:
            i = what.find(k)
            if i == -1:
                pass
#self.molecules.add(what)
            else:
                while i > -1:
                    for v in self.replacements[k]:
                        print('found', k, v, 'in', what, 'at', i)
                        adding = what[:i] + v + what[i + len(v) - 2:]
                        print(what[:i], v, what[i + len(v) - 2:])
                        self.molecules.add(adding)
                    i = what.find(k, i + 1)

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
#    print(p.molecules)

if __name__ == '__main__':
    main()


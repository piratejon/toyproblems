'''
try to solve 2015.19b in TYOOL 2017
'''

import sys

class Replacement:
    '''One replacement entry.'''

    def __init__(self, replacement_string):
        '''set me up'''
        self.left, self.right = replacement_string.split(' => ')

    def apply(self, string):
        '''generate all the single applications of this replacements'''
        splits = string.split(self.left)

        for i in range(len(string) - len(self.left) + 1):
            if string[i:].startswith(self.left):
                yield string[:i] + self.right + string[i + len(self.left):]

    def __repr__(self):
        return '{} => {}'.format(self.left, self.right)

class ReversedReplacement(Replacement):
    '''a reversed replacement entry'''

    def __init__(self, replacement_string):
        '''set me up'''
        self.right, self.left = replacement_string.split(' => ')


class Replacer:
    '''Does replacements to a molecule.'''

    def __init__(self):
        '''setup an empty replacement dict'''
        self.replacements = {}

    def add_replacement(self, replacement_string, constructor = Replacement):
        '''add a replacement to our collection'''
        replacement = constructor(replacement_string)
        if not replacement.left in self.replacements:
            self.replacements[replacement.left] = set()
        self.replacements[replacement.left].add(replacement)

class ReplacerIterator:
    '''
    Iterates over the items in a Replacer, so you could for example
    try each one or something.
    '''

    def __init__(self, replacements):
        '''turn the set into a list'''
        self.replacements = [
            repl
            for left, rights in replacements.replacements.items()
            for repl in rights
        ]
        self.i = len(self.replacements)

    def __iter__(self):
        '''look ma no hands!'''
        return self

    def __next__(self):
        '''gotta keep it fresh'''
        if self.i == 0:
            raise StopIteration
        self.i -= 1
        return self.replacements[self.i]

def Part1(filename):
    '''do part 1'''
    replacer = Replacer()
    with open(filename, 'r') as f:
        for line in f:
            if line != '\n':
                replacer.add_replacement(line.strip(), constructor = Replacement)
            else:
                break
        molecule = next(f).strip()

    print(molecule)

    unique_results = set()
    for r in ReplacerIterator(replacer):
        for new_molecule in r.apply(molecule):
            print(r, new_molecule)
            unique_results.add(new_molecule)

    print(len(unique_results))

def main(args):
    if len(args) == 0:
        Part1('testinput')
    for arg in args:
        Part1(arg)

if __name__ == '__main__':
    main(args = sys.argv[1:])

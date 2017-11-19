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

    def __init__(self, filename, constructor=Replacement):
        '''setup replacements from a file'''
        self.replacements = {}
        self.replacement_constructor = constructor

        with open(filename, 'r') as fin:
            for line in fin:
                if line != '\n':
                    self.add_replacement(line.strip())
                else:
                    break
            self.molecule = next(fin).strip()

    def add_replacement(self, replacement_string):
        '''add a replacement to our collection'''
        replacement = self.replacement_constructor(replacement_string)
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

def part1(filename):
    '''do part 1'''

    replacer = Replacer(filename)

    print(replacer.molecule)

    unique_results = set()
    for replacement in ReplacerIterator(replacer):
        for new_molecule in replacement.apply(replacer.molecule):
            print(replacement, new_molecule)
            unique_results.add(new_molecule)

    print(len(unique_results))

def part2_BFS(filename):
    '''do part 2 - iterative BFS
    this approach will probably arrive at the correct answer, given infinite tape
    '''
    replacer = Replacer(filename, constructor=ReversedReplacement)
    target = 'e'

    print(replacer.molecule)

    worked = {replacer.molecule: 0} # molecule => steps to derive
    unworked = set([replacer.molecule])

    while unworked:
        # print('stack size', len(unworked), unworked, worked)
        molecule = unworked.pop()
        for replacement in ReplacerIterator(replacer):
            for new_molecule in replacement.apply(molecule):
                if new_molecule not in worked:
                    if new_molecule != molecule:
                        unworked.add(new_molecule)
                    worked[new_molecule] = worked[molecule] + 1
                worked[new_molecule] = min(worked[molecule] + 1, worked[new_molecule])

    print(target, worked[target])

def part2_DFS(filename):
    '''do part 2, DFS so we don't need so much memory'''

    replacer = Replacer(filename, constructor=ReversedReplacement)
    target = 'e'

    print(replacer.molecule)

    def recursive_dfs_search(molecule, depth = 0):
        '''explore children before siblings'''
        #print(depth, molecule)
        if molecule == target:
            print(target, depth, worked[target])
        for replacement in ReplacerIterator(replacer):
            for new_molecule in replacement.apply(molecule):
                if new_molecule not in worked:
                    worked[new_molecule] = worked[molecule] + 1
                    if new_molecule != molecule:
                        recursive_dfs_search(new_molecule, depth + 1)
                worked[new_molecule] = min(worked[molecule] + 1, worked[new_molecule])

    worked = {replacer.molecule: 0}

    recursive_dfs_search(replacer.molecule)

    print(target, worked[target])

def part2(filename):
    return part2_DFS(filename)

def main(args):
    '''driver'''
    if len(args) == 0:
        part2('testinput')
    for arg in args:
        part2(arg)

if __name__ == '__main__':
    main(args=sys.argv[1:])

'''
try to solve 2015.19b in TYOOL 2017
'''

import random
import sys
import re

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

def part2_bfs(filename):
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

def part2_dfs(filename):
    '''do part 2, DFS so we don't need so much memory'''

    replacer = Replacer(filename, constructor=ReversedReplacement)
    target = 'e'

    print(replacer.molecule)

    def recursive_dfs_search(molecule, depth=0):
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

class ReplacementSmart(Replacement):
    '''a smart replacement, with the RHS decomposed into LHS and terminals'''

    decomposition_expression = re.compile('[A-Z][a-z]*')

    def __init__(self, string):
        super().__init__(string)
        self.decomposition = None

    @staticmethod
    def bare_decompose(right):
        '''just try the decompose without all the checks'''
        return ReplacementSmart.decomposition_expression.findall(right)

    def decompose(self, lefts):
        '''decompose RHS based on the provided LHS non-terminals'''
        # find RHS that could be expanded by other productions
        # this list is sorted by construction
        leftsubs = []
        for i in range(len(self.right)):
            for _, left in lefts:
                if self.right[i:].startswith(left):
                    leftsubs.append((i, left))

        # ensure rule decomposition starting points are unique
        all_start_indices = [_[0] for _ in leftsubs]
        assert sorted(all_start_indices) == sorted(set(all_start_indices))

        # ensure no decomposition rules overlap
        # leftsubs are sorted by construction
        for i in range(len(leftsubs) - 1):
            assert ((leftsubs[i][0] + len(leftsubs[i][1])) <
                    (leftsubs[i + 1][0] + len(leftsubs[i + 1][1])))

        # given that starting points are unique and substitutions do not
        # overlap, we can now perform the unique decomposition
        decomposition = ReplacementSmart.bare_decompose(self.right)
        print(self, decomposition)

        # make sure the parts equal the whole
        assert self.right == ''.join(decomposition)

        # make sure we used all the parts
        for sub in leftsubs:
            assert sub[1] in decomposition
        assert len(leftsubs) <= len(decomposition)

        self.decomposition = decomposition

class ReplacerSmart:
    '''hint from reddit that the input has some structure'''

    def __init__(self, filename=None):
        '''make blank/empty everything, load from file if it's there'''
        self.replacements = []
        self.molecule = None

        if filename:
            self.load_from_file(filename)

    def load_from_file(self, filename):
        '''populate our replacements and molecule from the file'''
        with open(filename, 'r') as fin:
            for line in fin:
                if line != '\n':
                    self.replacements.append(ReplacementSmart(line.strip()))
                else:
                    break
            self.molecule = next(fin).strip()

    def analyze_and_decompose(self):
        '''
        check various properties hold, and if so, decompose the RHS into
        terminals and non-terminals.

        the ReplacementSmart.decompose implements some additional checks
        '''

        # what appears on the right that does not appear on the left?
        lefts = {_.left for _ in self.replacements}
        lefts = [(i, left) for i, left in enumerate(lefts, 1)]

        # every left starts with a capital letter except 'e'
        for left in lefts:
            assert left[1] == 'e' or (left[1][0] >= 'A' and left[1][0] <= 'Z')

        # non-'e' lefts do not have capitals after the first letter
        for left in lefts:
            assert left[1] == 'e' or (left[1][1:].lower() == left[1][1:])

        for _ in self.replacements:
            _.decompose(lefts)

class SymbolDictionary:
    '''
    symbol dictionary with canonical labels and virtual symbol generator
    '''

    def __init__(self, symbols):
        '''initialize real symbols' canonical labels'''
        sorted_symbols = sorted(symbols)
        self.symbols = {_: i for i, _ in enumerate(sorted_symbols)}
        self.reverse = [(i, _, 'a' + str(i)) for i, _ in enumerate(sorted_symbols)]
        self.labels = {label: i for i, _, label in self.reverse}

    def lookup_label(self, key):
        '''retrieve the index of a relabeled symbol'''
        return self.labels[key]

    def lookup_symbol(self, key):
        '''retrieve the index of an original symbol'''
        return self.symbols[key]

    def __getitem__(self, index):
        '''retrieve the labels with the given index'''
        return self.reverse[index]

    def generate_label(self):
        '''generate a new virtual label'''
        label_id = len(self.labels)
        label = 'a' + str(label_id)
        self.reverse.append((label_id, None, label))
        self.labels[label] = label_id
        return (label, label_id)

    def __repr__(self):
        '''what do i look like to you'''
        return repr(self.reverse)

class ReplacerCNF:
    '''a CNF replacer (language) based on a ReplacerSmart'''

    def __init__(self, smart, start_symbol='e'):
        '''
        derive a ReplacerCNF from the ReplacerSmart
        following <https://en.wikipedia.org/wiki/Chomsky_normal_form>
        '''

        # figure out what symbols we currently have, and relabel to make adding
        # easier
        self.symbols = SymbolDictionary(
            {_.left for _ in smart.replacements} |
            {right for _ in smart.replacements for right in _.decomposition})

        print('symbols', self.symbols)

        # START: make sure start_symbol doesn't appear on the RHS
        for replacement in smart.replacements:
            assert start_symbol not in replacement.decomposition

        lefts = {_.left for _ in smart.replacements}
        # TERM: eliminate rules with nonsolitary terminals
        for replacement in smart.replacements:
            if len(set(replacement.decomposition) - lefts) > 0:
                print(replacement, "has non-solitary terminals", set(replacement.decomposition) - lefts)

def part2_smart(filename):
    '''try to do something a little better than brute force'''
    replacer = ReplacerSmart(filename)
    replacer.analyze_and_decompose()

    cnfreplacer = ReplacerCNF(replacer)

def part2_greedy(filename):
    '''try a greedy DFS match'''
    replacer = ReplacerSmart(filename)
    replacer.analyze_and_decompose()

    for replacement in replacer.replacements:
        print(replacement, replacement.decomposition)
    print(replacer.molecule)

    random.seed()
    counter = 0
    old_string = ''
    string = replacer.molecule
    while string != 'e':
        old_string = string
        replacement = random.choice(replacer.replacements)
        for i in range(len(string)):
            if string[i:].startswith(replacement.right):
                string = string[:i] + replacement.left + string[i + len(replacement.right):]
                counter += 1
                break
    print(counter, string)


def part2(filename):
    '''run one of our part2 strategies'''
    return part2_greedy(filename)

def main(args):
    '''driver'''
    if not args:
        part2('testinput')
    for arg in args:
        part2(arg)

if __name__ == '__main__':
    main(args=sys.argv[1:])

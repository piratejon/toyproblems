#!/usr/bin/env

import sys

ropelen, lengths = 5, [3, 4, 1, 5]
#ropelen, lengths = 256, [120,93,0,90,5,80,129,74,1,165,204,255,254,2,50,113]

llist = [_ for _ in range(ropelen)]

class Knot:
    '''implement a hashable knot'''

    def __init__(self, length=5):
        '''set us up an untwisted rope'''
        self.list = [_ for _ in range(length)]
        self.len = len(self.list)
        self.curpos = 0
        self.skip = 0

    def __getitem__(self, idx):
        # index modulo len(list)
        #print(idx)
        if type(idx).__name__ == 'slice':
            start, stop = idx.start, idx.stop
            start %= self.len
            stop %= self.len
            print(idx, start, stop)
            if start > stop: # wrapping
                #print('wrapping', idx)
                left = self.list[:stop]
                right = self.list[start:]
                middle = self.list[stop:start]
                #print(left, middle, right)
                return left + middle + right
            else:
                #print('nonwrapping', idx)
                return self.list[idx]
        elif type(idx) == 'int':
            return self.list[idx % self.len]

    def __setitem__(self, idx, val):
        if type(val).__name__ == 'slice':
            start, stop = idx.start, idx.stop
            start %= self.len
            stop %= self.len
            if start > stop: # wrapping
                left = self.list[:stop]
                right = self.list[start:]
                middle = self.list[stop:start]
                for i in range(len(val)):
                    if i < len(right):
                        right[i] = val[i]
                    else:
                        left[self.len - stop + i] = val[i]
                return left + middle + right
            else:
                for i in range(start, stop):
                    self.list[i % self.len] = val[i - start]
        elif type(val).__name__ == 'int':
            self.list[idx % self.len] = val

    def do_it(self, length):
        '''hash iter given this length'''
        if length < 0 or length > self.len:
            raise ValueError(length)
        print(self.list, self.curpos, self.skip, length)
        self.reverse(length)

        self.skip += 1
        self.curpos += length

    def reverse(self, length):
        '''reverse length items starting at curpos+skip'''
        revme = self[self.curpos + self.skip:self.curpos + self.skip + length]
        print('reverse', length, len(revme), revme)

def main(filename):
    '''driver'''
    if filename:
        with open(filename, 'r') as fin:
            ropelen, llist = 256, [int(_) for _ in fin.read().split(',')]
    else:
        ropelen, llist = 5, [3, 4, 1, 5]

    knot = Knot(ropelen)

    for _ in llist:
        knot.do_it(_)

if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) == 2 else None)

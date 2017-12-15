#!/usr/bin/env python3

import part10
import sys

sys.setrecursionlimit(9999)

key = 'wenycdww'
#key = 'flqrgnkx' # test

knotter = part10.Knot(256)

hashes = [knotter.hash('{}-{}'.format(key, str(i))) for i in range(128)]

barray = []
bits = 0
for khash in hashes:
    b = bin(int(khash, 16))[2:].zfill(128)
    bits += b.count('1')
    barray.append([_ == '1' for _ in b])

print('part 1:', bits)

grid = []

def fill_neighbors(b, i, j, group_id):
    '''assumes i,j is valid'''
    #print(b[i][j], i, j, group_id)

    b[i][j] = group_id

    if i > 0 and b[i-1][j] is True:
        #print('a', b[i-1][j])
        b = fill_neighbors(b, i - 1, j, group_id)

    if j > 0 and b[i][j-1] is True:
        #print('b', b[i][j-1])
        b = fill_neighbors(b, i, j - 1, group_id)

    if i < 127 and b[i+1][j] is True:
        #print('c', b[i+1][j])
        b = fill_neighbors(b, i + 1, j, group_id)

    if j < 127 and b[i][j+1] is True:
        #print('d', b[i][j+1])
        b = fill_neighbors(b, i, j + 1, group_id)

#    if i > 0 and j > 0 and b[i-1][j-1] is True:
#        #print('e', b[i-1][j-1])
#        b = fill_neighbors(b, i - 1, j - 1, group_id)
#
#    if i > 0 and j < 127 and b[i-1][j+1] is True:
#        #print('f', b[i-1][j+1])
#        b = fill_neighbors(b, i - 1, j + 1, group_id)
#
#    if i < 127 and j > 0 and b[i+1][j-1] is True:
#        #print('g', b[i+1][j-1])
#        b = fill_neighbors(b, i + 1, j - 1, group_id)
#
#    if i < 127 and j < 127 and b[i+1][j+1] is True:
#        #print('h', b[i+1][j+1])
#        b = fill_neighbors(b, i + 1, j + 1, group_id)

    return b

next_group = 0
for i in range(128):
    for j in range(128):
        if barray[i][j] is True:
            barray = fill_neighbors(barray, i, j, next_group)
            next_group += 1

print(next_group)

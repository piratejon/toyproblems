#!/usr/bin/env python3

def is_passphrase_valid_v1(p):
    '''do any duplicate words appear'''
    words = {}
    for _ in p:
        if _ not in words:
            words[_] = 0
        words[_] += 1
        if words[_] > 1:
            return False
    return True

def is_passphrase_valid_v2(p):
    '''do any words which are anagrams appear?'''
    anagrams = {}
    for _ in p:
        t = tuple(sorted(set(_)))
        if t not in anagrams:
            anagrams[t] = 0
        anagrams[t] += 1
        if anagrams[t] > 1:
            return False
    return True

with open('input', 'r') as fin:
    c0 = 0
    c1 = 0
    for line in fin:
        ph = line.split()
        c0 += 1 if is_passphrase_valid_v1(ph) else 0
        c1 += 1 if is_passphrase_valid_v2(ph) else 0
    print(c0, c1)

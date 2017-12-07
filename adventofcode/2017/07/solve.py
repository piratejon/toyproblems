#!/usr/bin/env python3

import sys

filename = 'test.input' if len(sys.argv) != 2 else sys.argv[1]

# part1
with open(filename, 'r') as fin:
    parents = {}
    for line in fin:
        parts = line.split('->')
        left = parts[0].split(' ')[0]
        if len(parts) > 1:
            rights = [_.strip() for _ in parts[1].split(', ')]
            for _ in rights:
                parents[_] = left
    print('part1', set(parents.values()) - set(parents.keys()))

# part2
def subtree_sum(root, tree, weights):
    x = weights[root]
    if root in tree:
        for child in tree[root]:
            x += subtree_sum(child, tree, weights)
    return x

def find_unbalanced_child(root, tree, weights):
    if root in tree:
        cw = {}
        for child in tree[root]:
            w = subtree_sum(child, tree, weights)
            if w not in cw:
                cw[w] = []
            cw[w].append(child)
        if len(cw.keys()) > 1:
            # assume two weights
            ws = sorted([(k, v) for k, v in cw.items()], key=lambda x: len(x[1]))
            print(ws)
            u = find_unbalanced_child(ws[0][1][0], tree, weights)
            if u:
                return u
            else:
                return (ws[0][1][0], weights[ws[0][1][0]], ws[0][0] - ws[1][0], weights[ws[0][1][0]] - (ws[0][0] - ws[1][0]))
    return None

def find_balanced_children(root, tree, weights):
    if root in tree:
        cw = {}
        for child in tree[root]:
            w = subtree_sum(child, tree, weights)
            if w not in cw:
                cw[w] = []
            cw[w].append(child)
            find_balanced_children(child, tree, weights)
        if len(cw.keys()) != 1:
            print(root, weights[root], 'unbalanced', cw)

with open(filename, 'r') as fin:
    parents = {}
    weights = {}
    for line in fin:
        parts = line.split('->')
        left = parts[0].split(' ')[0]
        weight = int(parts[0].split(' ')[1].replace('(', '').replace(')', ''))
        weights[left] = weight
        if len(parts) > 1:
            rights = [_.strip() for _ in parts[1].split(', ')]
            for _ in rights:
                parents[_] = left
    root = (set(parents.values()) - set(parents.keys())).pop()
    print(root, weights[root], sum(weights.values()))

    tree = {}
    for k, v in parents.items():
        if v not in tree:
            tree[v] = set()
        tree[v].add(k)

    find_balanced_children(root, tree, weights)
    print(find_unbalanced_child(root, tree, weights))

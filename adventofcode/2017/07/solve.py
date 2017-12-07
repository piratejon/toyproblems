#!/usr/bin/env python3

filename = 'input'

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
            for weight, children in cw.items():
                if len(children) == 1:
                    print(children[0], weights[children[0]], cw)
                    return find_unbalanced_child(children[0], tree, weights)

    return None

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

    print(find_unbalanced_child(root, tree, weights))

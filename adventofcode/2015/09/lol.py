#!/usr/bin/python3

import fileinput
import itertools

def compute_path_cost(path, costs):
    edges = [(path[i - 1], path[i]) for i in range(1, len(path))]
    return sum(costs[a][b] for a, b in edges)

def permute_paths(edges):
    vertices = frozenset(edges.keys())

    permuter = itertools.permutations(vertices, len(vertices))

    initial_path = next(permuter)

    best = (compute_path_cost(initial_path, edges), initial_path)

    for path in permuter:
        cost = compute_path_cost(path, edges)
        if cost < best[0]:
            best = (cost, path)

    return best

def parse_edge_cost(line, edges):
    start, _, end, _, distance = line.split()

    if start not in edges:
        edges[start] = {}

    if end not in edges:
        edges[end] = {}

    edges[start].update({end: int(distance) })
    edges[end].update({start: int(distance) })

if __name__ == '__main__':
    edges = {}
    for line in fileinput.input():
        parse_edge_cost(line, edges)

    print(permute_paths(edges))


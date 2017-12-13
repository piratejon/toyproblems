#!/usr/bin/env python3

import sys
import re

links = {}

def parse(line):
    '''turn the line into some tuple'''
    x = [int(_) for _ in re.split('[ ,<>-]', line) if _ != '']
    return (x[0], x[1:])

def add_link(a, b):
    '''add the single relationship to our list'''
    if a not in links:
        links[a] = set()
    links[a].add(b)

def add_links(rel):
    '''add the relationship to our links list'''
    for b in rel[1]:
        add_link(rel[0], b)
        add_link(b, rel[0])

def build_clique(e, clique):
    '''build the clique starting around element e'''
    clique.add(e)
    added = False
    for b in links[e]:
        if b not in clique:
            clique.add(b)
            clique = build_clique(b, clique)
            added = True
    return clique

def build_all_cliques():
    '''build all the cliques'''
    cliques = set()
    for a in links.keys():
        if not any(a in _ for _ in cliques):
            cliques.add(frozenset(build_clique(a, set())))
    return cliques

with open(sys.argv[1] if len(sys.argv) == 2 else 'test.input', 'r') as fin:
    for line in fin:
        add_links(parse(line))
    print(links)
    zero_clique = build_clique(0, set())
    print(len(zero_clique), zero_clique)
    all_cliques = build_all_cliques()
    print(len(all_cliques), all_cliques)


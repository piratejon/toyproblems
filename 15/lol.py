#!/usr/bin/python3

import fileinput

from scipy import optimize
import numpy as np

import random

def parse_line(line):
    w = line.split()
    return {
        w[0][:-1]: {
            w[i]: int(w[i + 1].split(',')[0]) for i in range(1, len(w), 2)
        }
    }

def count_calories(costs, qty):
    return sum([costs['calories'][k] * qty[k] for k in costs['calories']])

def score_up(costs, qty):
    weight = 1
    for factor in costs.keys():
        if factor != 'calories':
            weight *= max(0, sum([qty[k] * costs[factor][k] for k in qty]))
    return weight

def initial_guess(costs, ingreds, max_teaspoons, calorie_target):
    initial_score = 0
    while initial_score == 0:
        guesslist = [random.random() for _ in ingreds]
        scale = sum(guesslist)
        guesslist = [int(max_teaspoons * _ / scale) for _ in guesslist]
        guesslist[-1] = max_teaspoons - sum(guesslist[:-1])
        guess = {
#        _: int(a_hundred / len(ingreds)) for _ in ingreds
            k: v for k, v in zip(ingreds, guesslist)
        }
        initial_score = score_up(costs, guess)
    return guess

def calorie_walk(costs, ingreds, tsp_target, calorie_target):
    """bruteforce on things that satisfy both the teaspoon and calorie targets"""

    ordered_keys = sorted(ingreds)
    ordered_key_map = {i: v for i, v in enumerate(ordered_keys)}

    best_guess = {}
    best_weight = 0
    for i in range(pow(tsp_target + 1, len(ingreds))):
        guess = {ordered_key_map[_]: int(i / pow(tsp_target + 1, _)) % (tsp_target + 1) for _ in range(len(ingreds))}

        if sum(guess.values()) == tsp_target:
            if sum([v * costs['calories'][_] for _, v in guess.items()]) == calorie_target:
                weight = score_up(costs, guess)
                if weight > best_weight:
                    best_guess = guess
                    best_weight = weight
                    print('found new best', weight, 'from', guess)

    return best_guess

def goofy_ass_walk(costs, ingreds, max_teaspoons, calorie_target):
    """Try to increase things that improve the objective function?"""
    # make an initial guess that adds up to 100

    guess = initial_guess(costs, ingreds, max_teaspoons, calorie_target)
    initial_score = score_up(costs, guess)

    print('initial guess', guess, initial_score, count_calories(costs, guess))

    while True:
        initial_score = current_best = score_up(costs, guess)
        for i in ingreds:
            for j in ingreds:
                if i != j and guess[i] < max_teaspoons and guess[j] > 0:
                    guess[i] += 1
                    guess[j] -= 1
                    trial_score = score_up(costs, guess)
                    if trial_score > current_best:
                        current_best = trial_score
                        print('found new best', guess, current_best, count_calories(costs, guess))
                    else:
                        guess[i] -= 1
                        guess[j] += 1
        # don't loop forever if we did not make an improvement
        if initial_score == current_best:
            break

    return guess

def main():
    ingred = {}
    for line in fileinput.input():
        ingred.update(parse_line(line))

    costs = {}
    for k, v in ingred.items():
        for c, w in v.items():
            if c not in costs:
                costs[c] = {}
            costs[c][k] = w

    result = goofy_ass_walk(costs, ingred.keys(), 100, 500)
    print(result, score_up(costs, result)) 

    result = calorie_walk(costs, ingred.keys(), 100, 500)
    print(result, score_up(costs, result))

if __name__ == '__main__':
    main()


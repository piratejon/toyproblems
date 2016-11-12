#!/usr/bin/python3

import fileinput

import itertools

def add_em_up(sequence, matrix):
    score = 0
    for i in range(1, len(sequence)):
        score += matrix[sequence[i - 1]][sequence[i]]
        score += matrix[sequence[i]][sequence[i - 1]]

    score += matrix[sequence[0]][sequence[-1]]
    score += matrix[sequence[-1]][sequence[0]]

    return score

def line_em_up(matrix):
    guests = list(matrix.keys())

    permuter = itertools.permutations(matrix.keys(), len(matrix.keys()))

    best_list = next(permuter)
    best_score = add_em_up(best_list, matrix)

    for trial_list in permuter:
        trial_score = add_em_up(trial_list, matrix)
        if trial_score > best_score:
            best_score = trial_score
            best_list = trial_list

    return (best_list, best_score)

def parse_line(line):
    words = line[:-2].split()
    words[3] = int(words[3])
    if words[2] == 'lose':
        words[3] = - words[3]
    return (words[0], words[3], words[-1])

def main():
    matrix = {'me': {}}
    for line in fileinput.input():
        values = parse_line(line)
        if values[0] not in matrix:
            matrix[values[0]] = {}
        matrix[values[0]][values[2]] = values[1]
        matrix['me'][values[0]] = 0
        matrix[values[0]]['me'] = 0

    print(line_em_up(matrix))

if  __name__ == '__main__':
    main()


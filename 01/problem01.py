#!/usr/bin/python3

import sys

def problem01(string):
    floor = 0

    for ch in string:
        if ch == '(':
            floor += 1
        elif ch == ')':
            floor -= 1

    return floor

def main(args):
    """do the main thing"""
    print(problem01(open(args[0], 'r').read()))

if __name__ == '__main__':
    main(sys.argv[1:])


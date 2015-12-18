#!/usr/bin/python3

import sys

def main(args):
    """do the main thing"""

    with open(args[0], 'r') as fin:
        floor = 0
        while True:
            ch = fin.read(1)
            print(ch)
            if not ch:
                break
            if ch == '(':
                floor += 1
            elif ch == ')':
                floor -= 1
            else:
                print('skipping ', ch)
        print(floor)

if __name__ == '__main__':
    main(sys.argv[1:])


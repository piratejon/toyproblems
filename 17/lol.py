#!/usr/bin/python3

import fileinput

WINNERS=0

def container_it_up(c, goal, soln = []):
    global WINNERS
    print('entering', soln, c)
    for i, v in enumerate(c):
        container_it_up(c[i + 1:], goal)
        if sum(soln) + v < goal:
            soln.append(v)
            container_it_up(c[i + 1:], goal, soln)
        if sum(soln) == goal:
            WINNERS += 1

def main():
    containers = []
    for line in fileinput.input():
          if len(line.strip()) > 0:
              containers.append(int(line.strip()))

    print(containers)
    container_it_up(sorted(containers), 25)

    print(WINNERS)

if __name__ == '__main__':
    main()


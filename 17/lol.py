#!/usr/bin/python3

import fileinput

WINNERS=0

def container_it_up_listwise(c, goal, soln = []):
    global WINNERS
    for i, v in enumerate(c):
        soln.append(v)
        container_it_up_listwise(c[i + 1:], goal, soln)
        if sum(soln) == goal:
            WINNERS += 1
        soln.pop()

def main():
    containers = []
    for line in fileinput.input():
          if len(line.strip()) > 0:
              containers.append(int(line.strip()))

    print(containers)
    container_it_up_listwise(sorted(containers), 150)

    print(WINNERS)

if __name__ == '__main__':
    main()


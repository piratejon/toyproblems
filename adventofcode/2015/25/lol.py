#!/usr/bin/python3

class CodeIterator:
    M = 252533
    D = 33554393

    def __init__(self, n):
        self.n = n

    def __iter__(self):
        return self

    def __next__(self):
        self.n = int((self.n * CodeIterator.M) % CodeIterator.D)
        return self.n

class CantorIterator:
    def __init__(self, n=20151125):
        self.row = 1
        self.col = 1
        self.codeiter = CodeIterator(n)

    def __iter__(self):
        return self

    def __next__(self):
        if self.row == 1:
            self.row = self.col + 1
            self.col = 1
        else:
            self.row -= 1
            self.col += 1
        return (self.row, self.col, next(self.codeiter))

def p25_part1(row, column):
    ci = CantorIterator()
    while True:
        r = next(ci)
        if r[0] == row and r[1] == column:
            return r[2]

def main():
    print(p25_part1(2947, 3029))

if __name__ == '__main__':
    main()


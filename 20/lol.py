#!/usr/bin/python3

house = []

def prime_factors(n):
    i = 2
    factors = {}
    while i * i <= n:
        if n % i:
            i += 1
        else:
            n //= i
            if i not in factors:
                factors[i] = 0
            factors[i] += 1
    if n > 1:
        if n not in factors:
            factors[n] = 0
        factors[n] += 1
    return factors

def sum_factors(pf):
    factors = 1
    for p in pf:
        factors *= sum([p**i for i in range(pf[p] + 1)])
    return factors

def count_factors(pf):
    exps = 1
    for p in pf:
        exps *= (pf[p] + 1)
    return exps

def part1():
    goal = 34000000 / 10

    i = 1
    while True:
        pf = prime_factors(i)
        s = sum_factors(pf)
        c = count_factors(pf)
        if s >= goal:
            print(i, s, c, pf)
            break
        i += 1

def part2():
    goal = 34000000

    presents = {}

    elf_number = 1
    min_house_number = 9999999999999999999
    while elf_number <= goal / 2:
        # each elf does 50 deliveries
        # elf n delivers to houses n*[1..50]
        for elf_delivery in range(1, 51):
            house_number = elf_number * elf_delivery
            if house_number not in presents:
                presents[house_number] = 0
            presents[house_number] += (11 * elf_number)
#            print('elf', elf_number, 'delivering to', house_number, presents[house_number])
            if presents[house_number] >= goal:
                if house_number < min_house_number:
                    min_house_number = house_number
                    print('found new lowest house number', house_number, elf_number, presents[house_number])
        
        elf_number += 1

def main():
    part2()

if __name__ == '__main__':
    main()


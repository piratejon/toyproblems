#!/usr/bin/env python3

'''reverse captcha'''


# part1
with open('input', 'r') as fin:
    x = fin.read().strip()
print(x)

total = 0
for i in range(len(x)):
    if x[i] == x[i-1]:
        print(i, x[i], 'eq', x[i-1])
        total += int(x[i])
print(total)



# part 2
total = 0
l = int(len(x))
l2 = int(l/2)
for i in range(len(x)):
    if x[i] == x[(i + l2) % l]:
        total += int(x[i])
print(total)


#!/usr/bin/env python3

d = []
with open('input', 'r') as fin:
  for line in fin:
    nums = [int(_) for _ in line.split()]
    a, b = min(nums), max(nums)
    d.append(b - a)
print(sum(d))


d = []
with open('input', 'r') as fin:
  for line in fin:
    nums = sorted([int(_) for _ in line.strip().split()], reverse=True)
    for i in range(len(nums)):
      for j in range(i, len(nums)):
          if nums[i] % nums[j] == 0 and i != j:
            print(nums[i], nums[j], nums[i] / nums[j])
            d.append(nums[i] / nums[j])
print(sum(d))

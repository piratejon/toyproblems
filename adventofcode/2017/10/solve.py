#!/usr/bin/env

#llist = [_ for _ in range(5)]
#lengths = [3, 4, 1, 5]
llist = [_ for _ in range(256)]
lengths = [120,93,0,90,5,80,129,74,1,165,204,255,254,2,50,113]

curpos = 0
skipsize = 0

def hashsplice(l, start, end):
    end %= len(l)
    if end <= start: # wrapping scenario
        r = [_ for _ in reversed(l[start:] + l[:end])]
        ra, rb = r[:len(l[start:])], r[len(l[start:]):]
        return rb + l[end:start] + ra
    else:
        return l[:start] + [_ for _ in reversed(l[start:end])] + l[end:]

for length in lengths:
    llist = hashsplice(llist, curpos, curpos + length)
    curpos = (curpos + length + skipsize) % len(llist)
    skipsize += 1
print(llist[0] * llist[1], llist)

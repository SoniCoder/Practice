#!/bin/python3

import sys

n, m = input().strip().split(' ')
n, m = [int(n), int(m)]
c = list(map(int, input().strip().split(' ')))
# Print the number of ways of making change for 'n' units using coins having the values given by 'c'
ways = [0 for n_i in range(n+1)]
ways[0] = 1
for c_item in c:
    for n_i in range(c_item, n+1):
        ways[n_i] += ways[n_i - c_item]
    #print(ways)


print(ways[n])

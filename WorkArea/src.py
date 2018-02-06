#https://practice.geeksforgeeks.org/problems/egg-dropping-puzzle/0

import math

t = int(input())

def solve(n, k, dp):
    if dp[n][k] != -1: return dp[n][k]
    minres = math.inf
    for test_floor in range(1, k+1):
        maxcost = max(solve(n-1, test_floor-1, dp), solve(n, k-test_floor, dp))
        #print("Max Cost for %d eggs %d floors test floor: %d is %d"%(n, k, test_floor, maxcost))
        if maxcost < minres : minres = maxcost
    dp[n][k] = 1 + minres
    return dp[n][k]
    
for t_i in range(t):
    n, k = map(int,input().split())
    dp = [[-1 for floor_i in range(k+1)] for egg_i in range(n+1)]
    for floor_i in range(k+1):
        dp[1][floor_i] = floor_i
    for egg_i in range(n+1):
        dp[egg_i][0] = 0
    ans = solve(n, k, dp)
    print(ans)

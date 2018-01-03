#https://practice.geeksforgeeks.org/problems/matrix-chain-multiplication/0

import math

t = int(input())

def solve(arr, x, y, dp):
    if dp[x][y] != -1:
        return dp[x][y]
    mincost = math.inf
    for breakp in range(x+1, y+1):
        thiscost = solve(arr, x, breakp-1, dp) + solve(arr, breakp, y, dp) + arr[x]*arr[breakp]*arr[y+1]
        if thiscost < mincost: mincost = thiscost
    dp[x][y] = mincost
    return mincost

for t_i in range(t):
    n = int(input())
    arr = list(map(int, input().split()))
    maxindmat = n-2
    dp = [[-1 for j in range(n-1)] for i in range(n-1)]
    for i in range(n-1):
        dp[i][i] = 0
    ans = solve(arr, 0, n-2, dp)
    print(ans)

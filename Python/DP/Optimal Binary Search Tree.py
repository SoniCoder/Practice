#https://practice.geeksforgeeks.org/problems/optimal-binary-search-tree/0

import math

t = int(input())

def solve(freqs, x, y, dp):
    if x > y: return 0
    if dp[x][y] != -1:
        return dp[x][y]
    basecost = sum(freqs[x:y+1])
    cost2 = math.inf
    for root in range(x, y+1):
        thiscost = solve(freqs, x, root-1, dp) + solve(freqs, root+1, y, dp)
        if thiscost < cost2: cost2 = thiscost
    dp[x][y] = basecost + cost2
    return dp[x][y]
    
for t_i in range(t):
    n = int(input())
    keys = list(map(int, input().split()))
    freqs = list(map(int, input().split()))
    dp = [[-1 for j in range(n)] for i in range(n)]
    for dp_i in range(n):
        dp[dp_i][dp_i] = freqs[dp_i]
    ans = solve(freqs, 0, n-1, dp)
    print(ans)

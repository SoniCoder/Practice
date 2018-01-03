#!/bin/python3

import sys

if __name__ == "__main__":
    t = int(input().strip())
    for a0 in range(t):
        n = int(input().strip())
        arr = list(map(int, input().strip().split(' ')))
        dp = [(0, 0) for dp_i in range(n)]
        for i in range(1, n):
            lastlo = dp[i-1][0]
            lasthi = dp[i-1][1]
            newlo = None
            if lastlo>lasthi+abs(arr[i-1]-1):
                newlo = lastlo
            else:
                newlo = lasthi+abs(arr[i-1]-1)
            newhi = None
            if lastlo+abs(arr[i]-1)>lasthi+abs(arr[i]-arr[i-1]):
                newhi = lastlo+abs(arr[i]-1)
            else:
                newhi = lasthi+abs(arr[i]-arr[i-1])
            dp[i] = (newlo, newhi)
        print(max(dp[n-1][0], dp[n-1][1]))

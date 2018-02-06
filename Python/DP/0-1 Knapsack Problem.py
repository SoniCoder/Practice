#https://practice.geeksforgeeks.org/problems/0-1-knapsack-problem/0

t = int(input())

for t_i in range(t):
    n = int(input())
    W = int(input())
    vals = list(map(int, input().split()))
    weights = list(map(int, input().split()))
    dp = [[0 for W_i in range(W+1)] for item_i in range(n + 1)]
    for W_i in range(W+1): dp[0][W_i] = 0
    for item_i in range(n + 1): dp[item_i][0] = 0

    for item_i in range(1, n + 1):
        for W_i in range(1, W+1):
            if weights[item_i - 1] <= W_i:
                dp[item_i][W_i] = max(vals[item_i - 1] + dp[item_i - 1][W_i-weights[item_i - 1]], dp[item_i-1][W_i])
            else:
                dp[item_i][W_i] = dp[item_i - 1][W_i]

    print(dp[n][W])            

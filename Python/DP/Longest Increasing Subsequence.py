#https://practice.geeksforgeeks.org/problems/longest-increasing-subsequence/0

t = int(input())

for t_i in range(t):
    n = int(input())
    arr = list(map(int, input().split()))
    dp = [1 for i in range(n)]
    for j in range(1, n):
        for i in range(0, j):
            if arr[i] < arr[j]:
                dp[j] = max(dp[j], dp[i] + 1)
    print(max(dp))

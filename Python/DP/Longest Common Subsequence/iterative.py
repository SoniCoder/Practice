t = int(input())

for t_i in range(t):
    size1, size2 = map(int, input().split())
    str1 = input()
    str2 = input()
    dp = [[0 for s1_i in range(size1+1)] for s2_i in range(size2+1)]
    for s2_i in range(size2+1):
        dp[s2_i][0] = 0
    for s1_i in range(size1+1):
        dp[0][s1_i] = 0

    for s2_i in range(1, size2+1):
        for s1_i in range(1, size1+1):
            if str1[s1_i - 1] == str2[s2_i - 1]:
                dp[s2_i][s1_i] = dp[s2_i - 1][s1_i - 1] + 1
            else:
                dp[s2_i][s1_i] = max(dp[s2_i-1][s1_i], dp[s2_i][s1_i-1])
    print(dp[size2][size1])

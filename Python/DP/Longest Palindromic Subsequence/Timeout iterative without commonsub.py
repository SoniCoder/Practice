#https://practice.geeksforgeeks.org/problems/longest-palindromic-subsequence/0

t = int(input())

for t_i in range(t):
    str1 = input().lower()
    size1 = len(str1)
    dp = [[-1 for s1_i in range(size1)] for s2_i in range(size1)]
    for s_i in range(size1):
        dp[s_i][s_i] = 1
        
    for length in range(2, size1+1):
        for s_i in range(0, size1-length+1):
            if length == 2:
                if str1[s_i] == str1[s_i+length-1]:
                    dp[s_i][s_i+length-1] = 2
                else:
                    dp[s_i][s_i+length-1] = 1
            else:
                if str1[s_i] == str1[s_i+length-1]:
                    dp[s_i][s_i+length-1] = 2 + dp[s_i+1][s_i+length-2]
                else:
                    dp[s_i][s_i+length-1] = max(dp[s_i+1][s_i+length-1], dp[s_i][s_i+length-2])
    print(dp[0][size1-1])
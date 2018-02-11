#https://practice.geeksforgeeks.org/problems/edit-distance/0

t = int(input())

for t_i in range(t):
    size1, size2 = map(int, input().split())
    str1, str2 = input().split()
    dp = [[-1 for str1_i in range(size1+1)] for str2_i in range(size2+1)]
    for dp_i in range(size1+1):
        dp[0][dp_i] = dp_i
    for dp_i in range(size2+1):
        dp[dp_i][0] = dp_i
    for dp_i in range(1, size2+1):
        for dp_j in range(1, size1+1):
            if str1[dp_j-1] == str2[dp_i-1]: dp[dp_i][dp_j] = dp[dp_i-1][dp_j-1]
            else:
                dp[dp_i][dp_j] = 1 + min(dp[dp_i-1][dp_j-1], dp[dp_i-1][dp_j], dp[dp_i][dp_j-1])
    print(dp[size2][size1])
    

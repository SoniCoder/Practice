#https://practice.geeksforgeeks.org/problems/longest-palindromic-subsequence/0

t = int(input())

def solve(str1, str2, size1, size2, dp):
    if dp[size2][size1] != -1: return dp[size2][size1]
    if str1[size1-1] == str2[size2-1]:
        ans1 = 1 + solve(str1, str2, size1-1, size2-1, dp)
        dp[size2][size1] = ans1
        return ans1
    else:
        ans2 = solve(str1, str2, size1, size2-1, dp)
        ans3 = solve(str1, str2, size1-1, size2, dp)
        ans4 = max(ans2,ans3)
        dp[size2][size1] = ans4
        return ans4
        

for t_i in range(t):
    str1 = input()
    str2 = ''.join(reversed(str1))
    size1 = len(str1)
    size2 = size1
    dp = [[-1 for s1_i in range(size1+1)] for s2_i in range(size2+1)]
    for s2_i in range(size2+1):
        dp[s2_i][0] = 0
    for s1_i in range(size1+1):
        dp[0][s1_i] = 0
        
    ans = solve(str1, str2, size1, size2, dp)

    print(ans)

#https://practice.geeksforgeeks.org/problems/longest-palindromic-subsequence/0

t = int(input())

def solve(str1, x, y, dp):
    if x > y: return 0
    if dp[x][y] != -1: return dp[x][y]
    if str1[x] == str1[y]:
        dp[x][y] = 2 + solve(str1, x+1, y-1, dp) 
    else:
        ans1 = solve(str1, x+1, y, dp)
        ans2 = solve(str1, x, y-1, dp)
        dp[x][y] = max(ans1, ans2)
    return dp[x][y]
for t_i in range(t):
    str1 = input()
    size1 = len(str1)
    dp = [[-1 for s1_i in range(size1)] for s2_i in range(size1)]
    for s_i in range(size1):
        dp[s_i][s_i] = 1
        
    ans = solve(str1, 0, size1-1, dp)

    print(ans)

//https://practice.geeksforgeeks.org/problems/matrix-chain-multiplication/0

#include <iostream>
#include <vector>
#include <climits>

using namespace std;

int solve(vector<int>& arr, int x, int y, vector<vector<int>>& dp){
    if (dp[x][y] != -1) return dp[x][y];
    int mincost = INT_MAX;
    for(int breakp=x+1; breakp < y+1; ++breakp){
        int thiscost = solve(arr, x, breakp-1, dp) + solve(arr, breakp, y, dp) + arr[x]*arr[breakp]*arr[y+1];
        if (thiscost < mincost) mincost = thiscost;
    }
    dp[x][y] = mincost;
    return mincost;
}

int main(){
    int t; scanf("%d", &t);
    while(t--){
        int n; scanf("%d", &n);
        vector<int> arr(n);
        for(int n_i=0; n_i < n; ++n_i) scanf("%d", &arr[n_i]);
        vector<vector<int>> dp(n-1, vector<int>(n-1, -1));
        for(int i=0; i<n-1; ++i) dp[i][i] = 0;
        int ans = solve(arr, 0, n-2, dp);
        printf("%d\n", ans);
    }    
    return 0;
}


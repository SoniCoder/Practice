#include <iostream>
#include <vector>

using namespace std;

bool solve(vector<int> arr, int n, int k, vector<vector<bool>>& dp, vector<vector<bool>>& visited){
    if (visited[n][k]) return dp[n][k];
    
    if(arr[n-1]<=k){
    bool ans1 = solve(arr, n-1, k-arr[n-1], dp, visited);
        if(ans1){
            visited[n][k] = true;
            dp[n][k] = true;
            return true;
        }
    }
    bool ans2 = solve(arr, n-1, k, dp, visited);
    visited[n][k] = true;
    dp[n][k] = ans2;
    return ans2;
}


int main(){
    int t; scanf("%d", &t);
    while(t--){
        int n; scanf("%d", &n);
        vector<int> arr(n, 0);
        for(int n_i = 0; n_i <n; ++n_i) scanf("%d", &arr[n_i]);
        int k2 = 0;
        for(int n_i = 0; n_i <n; ++n_i) k2 += arr[n_i];
        if (k2%2){
            printf("NO\n");
            continue;
        }else{
            int k = k2/2;
            vector<vector<bool>> dp(n+1, vector<bool>(k+1, false));
            vector<vector<bool>> visited(n+1, vector<bool>(k+1, false));
            
            for(int item_i = 0; item_i < n+1; ++item_i) {
                dp[item_i][0] = true;
                visited[item_i][0] = true;
            }
            
            for(int dp_i = 0; dp_i < k+1; ++dp_i) {
                visited[0][dp_i] = true;
            }
            
            bool ans = solve(arr, n, k, dp, visited);
            
            if (ans) printf("YES\n");
            else printf("NO\n");
        } 
    }
    return 0;
}

//https://practice.geeksforgeeks.org/problems/subset-sum-problem/0

#include <iostream>
#include <vector>

using namespace std;

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
            for(int item_i = 0; item_i < n+1; ++item_i) dp[item_i][0] = true;
            bool found = false;
            for(int dp_i = 1; dp_i < k+1; ++dp_i){
                if (found) break;
                for(int arr_i = 1; arr_i < n+1; ++arr_i){
                    if(dp_i >= arr[arr_i-1]){
                        dp[arr_i][dp_i] = dp[arr_i-1][dp_i] || dp[arr_i-1][dp_i-arr[arr_i-1]];
                    }else{
                        dp[arr_i][dp_i] = dp[arr_i-1][dp_i];
                    }
                    if (dp[n][k]) {
                        printf("YES\n");
                        found = true;
                        break;
                    }
                }
            }
            if (found) continue;
            if (dp[n][k]) printf("YES\n");
            else printf("NO\n");
        } 
    }
    return 0;
}

//https://practice.geeksforgeeks.org/problems/longest-increasing-subsequence/0

#include <iostream>
#include <vector>

using namespace std;

int main(){
    int t; scanf("%d", &t);
    while(t--){
        int n; scanf("%d", &n);
        vector<int> arr(n);
        for(int n_i = 0; n_i < n; ++n_i)    scanf("%d", &arr[n_i]);
        if (n==0){
            printf("0\n");
            continue;
        }
        vector<int> dp(n, 1);
        for(int j = 1; j < n; ++j){
            for(int i = 0; i < j; ++i){
                if (arr[i] < arr[j]) dp[j] = max(dp[j], dp[i] + 1); 
            }
        }
        int maxi = 0;
        for(int dp_i = 0; dp_i < n; ++dp_i) maxi = max(maxi, dp[dp_i]);
        printf("%d\n", maxi);
    }
    return 0;
}
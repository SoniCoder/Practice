#include <stdio.h>
#include <vector>


void fastscan(int &number)
{
    bool negative = false;
    register int c;
    number = 0;
    c = getchar_unlocked();
    while(c!='-' && (c<48 || c>57)) c = getchar_unlocked();
    if (c=='-')
    {
        negative = true;
        c = getchar_unlocked();
    }
    for (; (c>47 && c<58); c=getchar_unlocked())
        number = number *10 + c - 48;
    if (negative)
        number *= -1;
}
 
using namespace std;

int main()
{
    int t;	fastscan(t);
    while(t--){
    	int n; fastscan(n);
        vector<vector<int>> arr(n, vector<int>(n, 0));
        for(int row_i = 0; row_i < n; ++row_i){
            for(int col_i = 0; col_i < n; ++col_i){
                fastscan(arr[row_i][col_i]);
            }
        }

        vector<vector<long long>> dp(n, vector<long long>(n, -1));
        for(int col_i = 0; col_i < n; ++col_i){
            dp[0][col_i] = arr[0][col_i];
        }
    
        long long lastmax = -1;
        int lastnum = -1;

        for(int col_i = 0; col_i < n; ++col_i){
            if (dp[0][col_i] > lastmax){
                lastmax = dp[0][col_i];
                lastnum = arr[0][col_i];
            }
        }

        for(int row_i = 1; row_i < n; ++row_i){
            for(int col_i = 0; col_i < n; ++col_i){
                int thisnum = arr[row_i][col_i];
                if (thisnum > lastnum){
                    dp[row_i][col_i] = thisnum + lastmax;
                    continue;
                }
                for(int col_j = 0; col_j < n; ++col_j)
                    if (thisnum > arr[row_i-1][col_j])
                        if (dp[row_i][col_i] < dp[row_i-1][col_j] + thisnum)
                            dp[row_i][col_i] = dp[row_i-1][col_j] + thisnum;
            }

            for(int col_i = 0; col_i < n; ++col_i){
                if (dp[row_i][col_i] > lastmax){
                    lastmax = dp[row_i][col_i];
                    lastnum = arr[row_i][col_i];
                }
            }
        }
        
        // int gmax = dp[n-1][0];        

        // for(int col_i = 1; col_i < n; ++col_i){
        //     if (dp[n-1][col_i] > gmax)
        //         gmax = dp[n-1][col_i];
        // }
        

        printf("%lld\n", lastmax);

    }
    
    return 0;
}
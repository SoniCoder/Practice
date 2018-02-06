//https://practice.geeksforgeeks.org/problems/longest-palindromic-subsequence/0

#include <iostream>
#include <vector>
#include <string.h>

using namespace std;

char *strrev(char *str)
{
      char *p1, *p2;

      if (! str || ! *str)
            return str;
      for (p1 = str, p2 = str + strlen(str) - 1; p2 > p1; ++p1, --p2)
      {
            *p1 ^= *p2;
            *p2 ^= *p1;
            *p1 ^= *p2;
      }
      return str;
}

int main(){
    int t; scanf("%d", &t); cin.ignore();
    while(t--){
        char str1[1001]; scanf("%s", str1); cin.ignore();
        char str2[1002]; strcpy(str2, str1); strrev(str2);
		//printf("%s\n%s\n", str1, str2);
        int size1 = strlen(str1);
        int size2 = size1;
        vector<vector<int>> dp(size2+1, vector<int>(size1+1, 0));
        for(int s2_i=0; s2_i<size2+1; ++s2_i) dp[s2_i][0] = 0;
        for(int s1_i=0; s1_i<size1+1; ++s1_i) dp[0][s1_i] = 0;
        for(int s2_i=1; s2_i<size2+1; ++s2_i){
            for(int s1_i=1; s1_i<size1+1; ++s1_i){
                if (str1[s1_i - 1] == str2[s2_i - 1]) dp[s2_i][s1_i] = dp[s2_i - 1][s1_i - 1] + 1;
                else dp[s2_i][s1_i] = max(dp[s2_i-1][s1_i], dp[s2_i][s1_i-1]);
            }
        }
        printf("%d\n", dp[size2][size1]);
    }
    return 0;
}	
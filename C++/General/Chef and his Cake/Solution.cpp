#include <iostream>
#include <vector>
#include <string>
using namespace std;

int main() {
	int t;
	scanf("%d", &t);
	cin.ignore();
	while(t--){
	    int n, m;
	    scanf("%d%d", &n, &m);
	    cin.ignore();
	    
	    vector<string> grid;
	    
	    for(int r_i = 0; r_i < n; ++r_i){
	        string s;
	        cin >> s;
	        grid.push_back(s);
	        cin.ignore();
	    }
	    
	    int sum1 = 0;
	    int sum2 = 0;
	    
	    for(int r_i = 0; r_i < n; ++r_i){
	        for(int c_i = 0; c_i < m; ++c_i){
	            if ((r_i + c_i) % 2){
	                if(grid[r_i][c_i] != 'G') sum1 += 5;
	                if(grid[r_i][c_i] != 'R') sum2 += 3;
	            }else{
	                if(grid[r_i][c_i] != 'R') sum1 += 3;
	                if(grid[r_i][c_i] != 'G') sum2 += 5;
	            }
	        }    
	    }
	    
	    int lesser = (sum1 < sum2)?sum1:sum2;
	    
	    
	    printf("%d\n", lesser);
	}
// 	cout << cin.peek() << "Hello";
	return 0;
}

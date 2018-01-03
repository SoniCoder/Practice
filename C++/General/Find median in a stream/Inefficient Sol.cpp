#include <iostream>
#include <vector>

using namespace std;

int main(){
	int n; scanf("%d", &n);
	vector<int> arr;
	for(int n_i=1; n_i <= n; ++n_i){
		int tmp; scanf("%d", &tmp);
		arr.push_back(tmp);
		int i = arr.size() - 1;
		while(i > 0){
			if (arr[i] < arr[i-1]){
				int tmp1 = arr[i];
				arr[i] = arr[i-1];
				arr[i-1] = tmp1;
			}
			--i;
		}
		if (n_i % 2){
			printf("%d\n", arr[(n_i + 1)/2 - 1]); 
		}else{
			printf("%d\n", (arr[n_i/2 - 1] + arr[n_i/2]) / 2);
		}
	}	
	return 0;
}
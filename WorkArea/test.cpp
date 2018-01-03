#include <iostream>
//#include <stdio.h>

int main(){
	std::ios_base::sync_with_stdio(false);
	std::cin.tie(NULL);
	int tmp;
	for(int i = 0; i < 100000000; ++i)
		std::cin >> tmp;
	//	printf("%d\n", tmp);
	return 0;
}

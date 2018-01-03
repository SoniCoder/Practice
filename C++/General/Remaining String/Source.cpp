#include <iostream>
#include <string.h>

using namespace std;

void solve(char* str1, int pos, char ch, int c, int len){
	if (pos<len - 1){
		if (c){
			if (str1[pos] == ch){
				c -= 1;
			}
		}else{
			printf("%c", str1[pos]);
		}
		solve(str1, pos + 1, ch, c, len);
	}else{
		if(c){
			printf("Empty string");
		}else{
			printf("%c", str1[pos]);
		}
	}
}

int main(){
	int t;
	scanf("%d", &t);
	cin.ignore();
	while(t--){
		char s[10001];
		scanf("%s", s); cin.ignore();
		char ch;
		scanf("%c", &ch); cin.ignore();
		int c;
		scanf("%d", &c); cin.ignore();
		solve(s, 0, ch, c, strlen(s));
		printf("\n");
	}
	return 0;
}
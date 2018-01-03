#include <iostream>
#include <vector>

using namespace std;

class TreeNode{
	public:
	TreeNode(int val):val(val), elements(1), left(NULL), right(NULL){};
	TreeNode* left;
	TreeNode* right;
	int val;
	int elements;
	void insert(int val){
		elements += 1;
		if (this->val >= val){
			if (right!=NULL) right->insert(val);
			else{
				right = new TreeNode(val);
			}
		}else{
			if (left!=NULL) left->insert(val);
			else{
				left = new TreeNode(val);
			}
		}
	}
	int getkth(int pos){
		int leftelements = 0;
		if (left) leftelements = left->elements;
		if (pos <= leftelements) return left->getkth(pos);
		if (pos == leftelements + 1) return val;
		else return right->getkth(pos - (leftelements + 1));
	}
	
};

int main(){
	int n; scanf("%d", &n);
	int num; scanf("%d", &num);
	TreeNode root(num);
	printf("%d\n", num);
	for(int n_i=2; n_i <= n; ++n_i){
		int num; scanf("%d", &num);
		root.insert(num);
		if(n_i%2){
			printf("%0.1f\n", (float)root.getkth((n_i+1)/2));
		}else{
			printf("%0.1f\n", ((float)root.getkth(n_i/2) + (float)root.getkth(n_i/2 + 1))/2);
		}	
	}
	
	
	// printf("%d\n", root.elements);
	
	return 0;
}
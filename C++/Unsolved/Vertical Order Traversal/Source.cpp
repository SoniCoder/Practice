/**
 * Definition for binary tree
 * struct TreeNode {
 *     int val;
 *     TreeNode *left;
 *     TreeNode *right;
 *     TreeNode(int x) : val(x), left(NULL), right(NULL) {}
 * };
 */
std::vector<std::vector<int>> leftList;
std::vector<std::vector<int>> rightList;

void postOrder(TreeNode* A, int level){
    if (A->left) postOrder(A->left, level - 1);
    if (A->right) postOrder(A->right, level + 1);
    
    if (level < 0){
        if (leftList.size() < abs(level)){
            std::vector<int>* newvec = new std::vector<int>();
            newvec->push_back(A->val);
            leftList.push_back(*newvec);
        }else{
            leftList[abs(level - 1)].push_back(A->val);
        }
    }
    else{
        if (rightList.size()<(level + 1)){
            std::vector<int>* newvec = new std::vector<int>();
            newvec->push_back(A->val);
            rightList.push_back(*newvec);
        }else{
            rightList[abs(level)].push_back(A->val);
        }
        
    }        
     
}
 
vector<vector<int> > Solution::verticalOrderTraversal(TreeNode* A) {
    postOrder(A, 0);
    std::vector<std::vector<int>> result;
    for(int i = leftList.size() - 1; i >= 0; --i){
        result.push_back(leftList[i]);
    }
    for(int i = 0; i <  rightList.size(); ++i){
        result.push_back(rightList[i]);
    }
    
    return result;
}

#include <iostream>
#include <vector>
#include <deque>
#include <set>

using namespace std;

class Node{
    public:
	int index;
    set<Node*> children;
	Node* parent;
};


int main(){
    int n, m;
    scanf("%d%d", &n, &m);
    cin.ignore();
    vector<Node*> nodeList(n+1);
	for(int index = 1; index < n + 1; ++index){
		nodeList[index] = new Node();
		nodeList[index]->index = index;
	}
	
	
    vector<bool> visited(n+1, false);
    for(int e_i = 0; e_i < m; ++e_i){
		int a, b;
		scanf("%d%d", &a, &b);
		// printf("Reading %d %d \n", a, b);
		cin.ignore();
		nodeList[a]->children.insert(nodeList[b]);
		nodeList[b]->children.insert(nodeList[a]);
	}
	// for(int node_i=1;node_i<=n;node_i++) for(set<Node*>::iterator it=nodeList[node_i]->children.begin();it!=nodeList[node_i]->children.end();it++) cout<<node_i<<"-"<<(*it)->index<<endl;
	// printf("Node 2 has %d children\n", (nodeList[2]->children).size());
    
	// for(set<Node*>::iterator it = nodeList[2]->children.begin(); it != nodeList[2]->children.end(); ++it){
		// printf("XXX\n");
	// }
	
	
	for(int node_i = 1; node_i < n + 1; ++node_i){
		if(!visited[node_i]){
			deque<int> stack1;
			stack1.push_back(node_i);
			Node* parent = NULL;
			nodeList[node_i]->parent = parent;
			while(!stack1.empty()){
				int item = stack1.back();
				stack1.pop_back();
				visited[item] = true;
				// nodeList[item]->parent = parent;
				for(set<Node*>::iterator it = nodeList[item]->children.begin(); it != nodeList[item]->children.end(); ++it){
					Node* child = *it;
					if (!visited[child->index]){
						child->parent = nodeList[item];
						//visited[child->index] = true;
						stack1.push_back(child->index);
					}					
				}
			}
		}
	}
	
	int happyverts = 0;
	
	for(int node_i = 1; node_i < n + 1; ++node_i){
		// for(set<Node*>::iterator it=nodeList[node_i]->children.begin();it!=nodeList[node_i]->children.end();it++) cout<<node_i<<"-"<<(*it)->index<<endl;
		// printf("Node %d has children %d\n", node_i, nodeList[node_i]->children.size());
		if (nodeList[node_i]->parent != NULL){
			if (nodeList[node_i]->parent->parent != NULL){
				if (nodeList[node_i]->children.size() > nodeList[node_i]->parent->children.size()) happyverts += 1;
			}else{
				if (nodeList[node_i]->children.size() - 1 > nodeList[node_i]->parent->children.size()) happyverts += 1;
			}
		}
	}
	
	printf("%d\n", happyverts);	
	
    return 0;
}
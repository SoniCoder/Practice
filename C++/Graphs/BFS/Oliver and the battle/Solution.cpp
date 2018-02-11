#include <iostream>
#include <vector>
#include <deque>

using namespace std;

int main() {
	int t;
	scanf("%d", &t);
	while (t--) {
		int n, m;
		int troops = 0, maxzom = 0;
		scanf("%d%d", &n, &m);
		vector<vector<int>> visited(n, vector<int>(m, 0));
		vector<vector<int>> graph(n, vector<int>(m, 0));
		deque<pair<int, int>> queue;
		for (int r_i = 0; r_i < n; ++r_i) {
			for (int c_i = 0; c_i < m; ++c_i) {
				int tmp;
				scanf("%d", &tmp);
				graph[r_i][c_i] = tmp;
			}
		}
		
		for (int r_i = 0; r_i < n; ++r_i) {
			for (int c_i = 0; c_i < m; ++c_i) {
				if (graph[r_i][c_i] == 1 && !visited[r_i][c_i]) {
					++troops;
					queue.push_back(pair<int,int>(r_i, c_i));
					visited[r_i][c_i] = 1;
					int thiszom = 0;
					while (!queue.empty()) {
						pair<int,int> item = queue.front();
						queue.pop_front();
						int x = item.first;
						int y = item.second;
						thiszom += 1;
						if ((y < m - 1) && !visited[x][y + 1] && graph[x][y+1]) {
							queue.push_back(pair<int, int>(x, y + 1));
							visited[x][y + 1] = 1;
						}
						if ((y < m - 1) && (x>0) && !visited[x-1][y + 1]&& graph[x-1][y+1]) {
							queue.push_back(pair<int, int>(x-1, y + 1));
							visited[x-1][y + 1] = 1;
						}
						if ((y < m - 1) && (x < n - 1) && !visited[x + 1][y + 1] && graph[x+1][y + 1]) {
							queue.push_back(pair<int, int>(x + 1, y + 1));
							visited[x + 1][y + 1] = 1;
						}
						if ((y > 0) && (x > 0) && !visited[x - 1][y - 1] && graph[x-1][y - 1]) {
							queue.push_back(pair<int, int>(x - 1, y - 1));
							visited[x - 1][y - 1] = 1;
						}
						if ((y > 0) && (x < n - 1) && !visited[x+1][y - 1] && graph[x+1][y - 1]) {
							queue.push_back(pair<int, int>(x+1, y - 1));
							visited[x + 1][y - 1] = 1;
						}
						if ((x > 0) && !visited[x - 1][y] && graph[x - 1][y]) {
							queue.push_back(pair<int, int>(x - 1, y));
							visited[x - 1][y] = 1;
						}
						if ((x < n - 1) && !visited[x+1][y] && graph[x+1][y]) {
							queue.push_back(pair<int, int>(x+1, y));
							visited[x+1][y] = 1;
						}
						if ((y > 0) && !visited[x][y-1] && graph[x][y-1]) {
							queue.push_back(pair<int, int>(x, y-1));
							visited[x][y-1] = 1;
						}
						
					}
					if (thiszom > maxzom){
						maxzom = thiszom;
					}
				}
			}
		}
		printf("%d %d\n", troops, maxzom);
	}

	return 0;
}
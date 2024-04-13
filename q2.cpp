class Solution {
public:
  vector<vector<int>> verticalTraversal(TreeNode* root) {

    int minx = 0, maxx = 0;
    dfs(root, 0, minx, maxx);

    int len = maxx - minx + 1;
    vector<vector<int>> v(len, vector<int>());

    bfs(root, 0-minx, v);
    return v;
  }

  void dfs(TreeNode* p, int x, int& minX, int& maxX) {
    minX = min(minX, x);
    maxX = max(maxX, x);

    if (NULL != p->left) dfs(p->left, x - 1, minX, maxX);
    if (NULL != p->right) dfs(p->right, x + 1, minX, maxX);
  }

  void bfs(TreeNode* root, int x, vector<vector<int>>& v) {
   typedef pair<int, TreeNode*> PAIR;
   queue<PAIR> q;
   q.push(PAIR(x, root));

   while (!q.empty()) {
     size_t len = q.size();
     map<int, vector<int>> m;
     for (size_t i = 0; i < len; i++) {
       PAIR p = q.front();
       q.pop();
       m[p.first].push_back(p.second->val);

       if (p.second->left != NULL) {
         q.push(PAIR(p.first - 1, p.second->left));
       }
       if (p.second->right != NULL) {
         q.push(PAIR(p.first + 1, p.second->right));
       }
     }

     for (auto iter = m.begin(); iter != m.end(); ++iter) {
       vector<int> nums = iter->second;
       sort(nums.begin(), nums.end());

       vector<int>& vv = v[iter->first];
       vv.insert(vv.end(), nums.begin(), nums.end());
     }
   }
 }

};

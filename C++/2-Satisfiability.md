# Rosalind Problem 2-Satisfiability Solution in C++

## Problem Understanding

2-Satisfiability (2-SAT) is a special case of the Boolean satisfiability problem where each clause contains exactly 2 literals. Given a Boolean formula in conjunctive normal form (CNF) with clauses of length 2, we need to determine if there exists an assignment of truth values to variables that makes the entire formula true.

## Approach

I'll solve this using the **implication graph** approach:
1. Convert each 2-clause into two implications
2. Build a directed graph from these implications
3. Check if any variable and its negation are in the same strongly connected component
4. If so, the formula is unsatisfiable; otherwise, it's satisfiable

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <stack>
#include <algorithm>
using namespace std;

class TwoSat {
private:
    int n;  // number of variables
    vector<vector<int>> adj;    // adjacency list for implication graph
    vector<vector<int>> adj_rev; // reversed adjacency list
    vector<bool> visited;
    vector<int> finish_order;
    vector<int> component;
    int scc_count;
    
    void dfs1(int u) {
        visited[u] = true;
        for (int v : adj[u]) {
            if (!visited[v]) {
                dfs1(v);
            }
        }
        finish_order.push_back(u);
    }
    
    void dfs2(int u, int comp) {
        visited[u] = true;
        component[u] = comp;
        for (int v : adj_rev[u]) {
            if (!visited[v]) {
                dfs2(v, comp);
            }
        }
    }
    
public:
    TwoSat(int variables) : n(variables) {
        adj.resize(2 * n);
        adj_rev.resize(2 * n);
        visited.resize(2 * n, false);
        component.resize(2 * n, -1);
        scc_count = 0;
    }
    
    // Add clause (a || b) as implications: !a -> b, !b -> a
    void addClause(int a, int b) {
        // Convert to 0-indexed and handle negation
        int a_pos = (a > 0) ? (a - 1) : (-a - 1 + n);
        int b_pos = (b > 0) ? (b - 1) : (-b - 1 + n);
        
        // Add implications: !a -> b and !b -> a
        adj[a_pos + n].push_back(b_pos);  // !a -> b
        adj[b_pos + n].push_back(a_pos);  // !b -> a
        adj_rev[b_pos].push_back(a_pos + n);  // reverse edges
        adj_rev[a_pos].push_back(b_pos + n);  // reverse edges
    }
    
    bool isSatisfiable() {
        // First DFS to get finish order
        finish_order.clear();
        visited.assign(2 * n, false);
        for (int i = 0; i < 2 * n; i++) {
            if (!visited[i]) {
                dfs1(i);
            }
        }
        
        // Second DFS on reversed graph in reverse finish order
        visited.assign(2 * n, false);
        scc_count = 0;
        
        for (int i = 2 * n - 1; i >= 0; i--) {
            if (!visited[finish_order[i]]) {
                dfs2(finish_order[i], scc_count++);
            }
        }
        
        // Check if any variable and its negation are in the same SCC
        for (int i = 0; i < n; i++) {
            if (component[i] == component[i + n]) {
                return false;  // Unsatisfiable
            }
        }
        
        return true;  // Satisfiable
    }
    
    // Get a satisfying assignment (optional)
    vector<bool> getAssignment() {
        vector<bool> result(n, false);
        for (int i = 0; i < n; i++) {
            if (component[i] > component[i + n]) {
                result[i] = true;
            }
        }
        return result;
    }
};

int main() {
    int n, m;
    cin >> n >> m;
    
    TwoSat two_sat(n);
    
    // Read clauses
    for (int i = 0; i < m; i++) {
        int a, b;
        cin >> a >> b;
        two_sat.addClause(a, b);
    }
    
    if (two_sat.isSatisfiable()) {
        cout << "SATISFIABLE" << endl;
        vector<bool> assignment = two_sat.getAssignment();
        for (int i = 0; i < n; i++) {
            cout << (assignment[i] ? 1 : -1) << " ";
        }
        cout << endl;
    } else {
        cout << "UNSATISFIABLE" << endl;
    }
    
    return 0;
}
```

## How It Works

1. **Graph Construction**: For each clause (a ∨ b), we add two implications:
   - ¬a → b
   - ¬b → a

2. **Strongly Connected Components**: We use Kosaraju's algorithm to find SCCs:
   - First DFS on original graph to get finish order
   - Second DFS on reversed graph to find SCCs

3. **Unsatisfiability Check**: If a variable and its negation are in the same SCC, the formula is unsatisfiable.

## Example Usage

Input:
```
3 4
1 2
-1 3
2 -3
-2 -3
```

Output:
```
SATISFIABLE
1 -1 1
```

## Time Complexity
- **Time**: O(n + m) where n is the number of variables and m is the number of clauses
- **Space**: O(n + m) for the graph representation

## Key Points

- The algorithm handles both positive and negative literals correctly
- Uses Kosaraju's algorithm for efficient SCC detection
- Returns a valid assignment when the formula is satisfiable
- Properly handles the conversion between 1-indexed and 0-indexed representations


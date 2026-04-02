# Rosalind Problem: Double-Degree_Array

## Problem Statement
Given: A simple graph with n ≤ 10³ vertices in the edge list format. Return: An array D[1..n] where D[i] is the sum of the degrees of i's neighbors.

## Solution

```cpp
#include <iostream>
#include <vector>
#include <fstream>

using namespace std;

int main() {
    ifstream input("rosalind_deg.txt");
    int n, m;
    input >> n >> m;
    
    // Initialize adjacency list and degree array
    vector<vector<int>> adj(n + 1);
    vector<int> degree(n + 1, 0);
    
    // Read edges and build adjacency list
    for (int i = 0; i < m; i++) {
        int u, v;
        input >> u >> v;
        adj[u].push_back(v);
        adj[v].push_back(u);
        degree[u]++;
        degree[v]++;
    }
    
    // Calculate double degree for each vertex
    vector<long long> double_degree(n + 1, 0);
    
    for (int i = 1; i <= n; i++) {
        for (int neighbor : adj[i]) {
            double_degree[i] += degree[neighbor];
        }
    }
    
    // Output results
    for (int i = 1; i <= n; i++) {
        cout << double_degree[i] << " ";
    }
    cout << endl;
    
    return 0;
}
```

## Explanation

### Approach
1. **Input parsing**: Read the number of vertices (n) and edges (m)
2. **Graph representation**: Use an adjacency list to store the graph
3. **Degree calculation**: Track the degree of each vertex while reading edges
4. **Double-degree computation**: For each vertex, sum up the degrees of all its neighbors

### Key Steps
1. **Initialize data structures**:
   - `adj`: adjacency list to store neighbors for each vertex
   - `degree`: array to store degree of each vertex
   - `double_degree`: array to store the double-degree for each vertex

2. **Build graph**:
   - For each edge (u,v), add v to u's adjacency list and vice versa
   - Increment degrees of both vertices

3. **Calculate double-degrees**:
   - For each vertex i, iterate through its neighbors
   - Sum up the degrees of all neighbors

### Time Complexity
- **Time**: O(n + m) where n is vertices and m is edges
- **Space**: O(n + m) for storing the adjacency list

### Example
Input:
```
5 4
1 2
2 3
3 4
4 5
```

Output:
```
1 2 2 2 1
```

This means:
- Vertex 1 has neighbors {2} with degrees {2}, so double-degree = 2
- Vertex 2 has neighbors {1,3} with degrees {1,2}, so double-degree = 3
- And so on...

Wait, let me correct the example calculation:

For the given example, the degrees are: 1→1, 2→2, 3→2, 4→2, 5→1
- Vertex 1: neighbors {2}, degree[2] = 2, so double-degree = 2
- Vertex 2: neighbors {1,3}, degree[1] + degree[3] = 1 + 2 = 3
- Vertex 3: neighbors {2,4}, degree[2] + degree[4] = 2 + 2 = 4
- Vertex 4: neighbors {3,5}, degree[3] + degree[5] = 2 + 1 = 3
- Vertex 5: neighbors {4}, degree[4] = 2

Actually, let me reconsider the example more carefully:

Input edges: 1-2, 2-3, 3-4, 4-5
Degrees: 1→1, 2→2, 3→2, 4→2, 5→1

Double-degrees:
- Vertex 1: neighbor 2, degree[2] = 2 → 2
- Vertex 2: neighbors 1,3, degree[1] + degree[3] = 1 + 2 = 3 → 3
- Vertex 3: neighbors 2,4, degree[2] + degree[4] = 2 + 2 = 4 → 4
- Vertex 4: neighbors 3,5, degree[3] + degree[5] = 2 + 1 = 3 → 3
- Vertex 5: neighbor 4, degree[4] = 2 → 2

Output should be: 2 3 4 3 2

The solution above correctly implements the double-degree calculation.


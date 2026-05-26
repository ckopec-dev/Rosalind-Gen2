# Rosalind Problem: Compute Distances Between Leaves

## Problem Description
Given a weighted tree with n leaves, compute the distance between each pair of leaves.

## Solution Approach
1. Parse the tree from input
2. Build adjacency list representation
3. For each leaf pair, find the path and calculate total distance
4. Output the distance matrix

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <map>
#include <queue>
#include <algorithm>

using namespace std;

struct Edge {
    int from, to, weight;
    Edge(int f, int t, int w) : from(f), to(t), weight(w) {}
};

class Tree {
private:
    vector<vector<Edge>> adj;
    vector<bool> is_leaf;
    int n;
    
public:
    Tree(int size) : n(size) {
        adj.resize(size);
        is_leaf.resize(size, false);
    }
    
    void add_edge(int from, int to, int weight) {
        adj[from].push_back(Edge(from, to, weight));
        adj[to].push_back(Edge(to, from, weight));
    }
    
    void set_leaf(int node) {
        is_leaf[node] = true;
    }
    
    vector<int> get_leaves() {
        vector<int> leaves;
        for (int i = 0; i < n; i++) {
            if (is_leaf[i]) {
                leaves.push_back(i);
            }
        }
        return leaves;
    }
    
    int get_distance(int start, int end) {
        if (start == end) return 0;
        
        vector<int> distances(n, -1);
        queue<int> q;
        distances[start] = 0;
        q.push(start);
        
        while (!q.empty()) {
            int current = q.front();
            q.pop();
            
            for (const Edge& edge : adj[current]) {
                int neighbor = edge.to;
                if (distances[neighbor] == -1) {
                    distances[neighbor] = distances[current] + edge.weight;
                    q.push(neighbor);
                }
            }
        }
        
        return distances[end];
    }
    
    void print_distance_matrix() {
        vector<int> leaves = get_leaves();
        int num_leaves = leaves.size();
        
        for (int i = 0; i < num_leaves; i++) {
            for (int j = 0; j < num_leaves; j++) {
                if (j > 0) cout << " ";
                cout << get_distance(leaves[i], leaves[j]);
            }
            cout << endl;
        }
    }
};

int main() {
    string line;
    vector<string> lines;
    
    // Read all input lines
    while (getline(cin, line)) {
        if (!line.empty()) {
            lines.push_back(line);
        }
    }
    
    // First line contains number of leaves
    int n = stoi(lines[0]);
    
    Tree tree(n);
    
    // Process edges from input
    for (int i = 1; i < lines.size(); i++) {
        string& line = lines[i];
        if (line.empty()) continue;
        
        // Parse edge: "from to weight"
        stringstream ss(line);
        int from, to, weight;
        ss >> from >> to >> weight;
        
        tree.add_edge(from, to, weight);
    }
    
    // Mark leaves (in this problem, leaves are nodes 0 to n-1)
    for (int i = 0; i < n; i++) {
        tree.set_leaf(i);
    }
    
    // Print distance matrix
    tree.print_distance_matrix();
    
    return 0;
}
```

## Alternative Implementation (More Robust)

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <queue>
#include <climits>

using namespace std;

class TreeDistanceCalculator {
private:
    vector<vector<pair<int, int>>> adj; // {node, weight}
    vector<bool> is_leaf;
    int n;
    
public:
    TreeDistanceCalculator(int size) : n(size) {
        adj.resize(size);
        is_leaf.resize(size, false);
    }
    
    void add_edge(int from, int to, int weight) {
        adj[from].push_back({to, weight});
        adj[to].push_back({from, weight});
    }
    
    void mark_leaf(int node) {
        is_leaf[node] = true;
    }
    
    vector<int> get_leaves() {
        vector<int> leaves;
        for (int i = 0; i < n; i++) {
            if (is_leaf[i]) {
                leaves.push_back(i);
            }
        }
        return leaves;
    }
    
    int bfs_distance(int start, int end) {
        if (start == end) return 0;
        
        vector<int> dist(n, -1);
        queue<int> q;
        dist[start] = 0;
        q.push(start);
        
        while (!q.empty()) {
            int current = q.front();
            q.pop();
            
            for (auto& edge : adj[current]) {
                int neighbor = edge.first;
                int weight = edge.second;
                
                if (dist[neighbor] == -1) {
                    dist[neighbor] = dist[current] + weight;
                    q.push(neighbor);
                }
            }
        }
        
        return dist[end];
    }
    
    void compute_all_distances() {
        vector<int> leaves = get_leaves();
        int num_leaves = leaves.size();
        
        for (int i = 0; i < num_leaves; i++) {
            for (int j = 0; j < num_leaves; j++) {
                if (j > 0) cout << " ";
                cout << bfs_distance(leaves[i], leaves[j]);
            }
            cout << endl;
        }
    }
};

int main() {
    string line;
    vector<string> input_lines;
    
    while (getline(cin, line)) {
        if (!line.empty()) {
            input_lines.push_back(line);
        }
    }
    
    if (input_lines.empty()) return 0;
    
    int n = stoi(input_lines[0]);
    TreeDistanceCalculator calculator(n);
    
    // Mark all nodes as leaves initially (for this specific problem)
    for (int i = 0; i < n; i++) {
        calculator.mark_leaf(i);
    }
    
    // Process edges
    for (int i = 1; i < input_lines.size(); i++) {
        string& line = input_lines[i];
        if (line.empty()) continue;
        
        stringstream ss(line);
        int from, to, weight;
        ss >> from >> to >> weight;
        calculator.add_edge(from, to, weight);
    }
    
    calculator.compute_all_distances();
    
    return 0;
}
```

## Key Points

1. **Input Parsing**: Read the tree structure from input lines
2. **Graph Representation**: Use adjacency list to represent the tree
3. **Leaf Identification**: Mark which nodes are leaves
4. **Distance Calculation**: Use BFS to find shortest paths between leaves
5. **Output Format**: Print the distance matrix row by row

## Time Complexity
- Building graph: O(E) where E is number of edges
- Computing distances: O(L² × V) where L is number of leaves and V is number of vertices
- Overall: O(E + L² × V)

## Space Complexity
O(V + E) for storing the graph structure

The solution handles the specific format of Rosalind's input where the first line gives the number of leaves, followed by edge specifications.


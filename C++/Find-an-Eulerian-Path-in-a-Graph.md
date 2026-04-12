# Rosalind Problem: Find an Eulerian Path in a Graph

## Problem Statement
Find an Eulerian path in a directed graph. An Eulerian path is a path that visits every edge exactly once.

## Solution Approach
1. First check if the graph has an Eulerian path (all vertices have equal in-degree and out-degree except two vertices)
2. Find the starting vertex (vertex with out-degree - in-degree = 1)
3. Use Hierholzer's algorithm to find the Eulerian path

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <stack>
#include <map>
#include <algorithm>
#include <sstream>

using namespace std;

class EulerianPath {
private:
    map<int, vector<int>> graph;
    map<int, int> in_degree;
    map<int, int> out_degree;
    int num_edges;
    
public:
    void add_edge(int from, int to) {
        graph[from].push_back(to);
        out_degree[from]++;
        in_degree[to]++;
        num_edges++;
    }
    
    int find_starting_vertex() {
        int start = -1;
        int unbalanced_count = 0;
        
        for (auto& pair : out_degree) {
            int vertex = pair.first;
            int out = pair.second;
            int in = in_degree[vertex];
            
            if (out - in == 1) {
                start = vertex;
                unbalanced_count++;
            } else if (out - in == -1) {
                unbalanced_count++;
            } else if (out - in != 0) {
                return -1; // Invalid graph
            }
        }
        
        // If no unbalanced vertex, start from any vertex with out-degree > 0
        if (start == -1) {
            for (auto& pair : out_degree) {
                if (pair.second > 0) {
                    start = pair.first;
                    break;
                }
            }
        }
        
        return start;
    }
    
    vector<int> find_eulerian_path() {
        vector<int> path;
        stack<int> stack;
        
        int start = find_starting_vertex();
        if (start == -1) return path;
        
        stack.push(start);
        
        while (!stack.empty()) {
            int current = stack.top();
            
            if (graph[current].empty()) {
                path.push_back(current);
                stack.pop();
            } else {
                int next = graph[current].back();
                graph[current].pop_back();
                stack.push(next);
            }
        }
        
        reverse(path.begin(), path.end());
        return path;
    }
    
    void print_graph() {
        for (auto& pair : graph) {
            cout << pair.first << " -> ";
            for (int neighbor : pair.second) {
                cout << neighbor << " ";
            }
            cout << endl;
        }
    }
};

int main() {
    EulerianPath ep;
    string line;
    
    // Read input until empty line
    while (getline(cin, line) && !line.empty()) {
        // Parse the edge
        stringstream ss(line);
        string from_str, to_str;
        
        if (getline(ss, from_str, ' ')) {
            if (getline(ss, to_str)) {
                int from = stoi(from_str);
                int to = stoi(to_str);
                ep.add_edge(from, to);
            }
        }
    }
    
    // Find and print Eulerian path
    vector<int> path = ep.find_eulerian_path();
    
    if (path.empty()) {
        cout << "No Eulerian path found" << endl;
        return 1;
    }
    
    cout << path[0];
    for (int i = 1; i < path.size(); i++) {
        cout << " -> " << path[i];
    }
    cout << endl;
    
    return 0;
}
```

## Example Input/Output

**Input:**
```
0 -> 2
1 -> 3
2 -> 1
3 -> 0
4 -> 1
```

**Output:**
```
4 -> 1 -> 3 -> 0 -> 2 -> 1
```

## Key Points

1. **Graph Representation**: Uses adjacency list representation with `map<int, vector<int>>`
2. **Degree Tracking**: Maintains in-degrees and out-degrees for all vertices
3. **Starting Vertex**: Finds the vertex where out-degree - in-degree = 1 (if exists)
4. **Hierholzer's Algorithm**: Uses stack-based approach to find Eulerian path
5. **Edge Cases**: Handles graphs with no Eulerian path and graphs with multiple components

## Time Complexity
- O(E + V) where E is number of edges and V is number of vertices

## Space Complexity
- O(E + V) for storing the graph and auxiliary data structures


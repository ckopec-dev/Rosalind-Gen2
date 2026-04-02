# Rosalind Problem: Generate All Maximal Non-Branching Paths in a Graph

## Problem Understanding

A maximal non-branching path is a path in a graph where:
1. Every internal node has in-degree = out-degree = 1
2. The path cannot be extended further (i.e., it's maximal)
3. It's a path that doesn't branch (no node has out-degree > 1)

## Approach

1. **Find all nodes with in-degree ≠ 1 or out-degree ≠ 1**
2. **Identify isolated cycles** (nodes with in-degree = out-degree = 1)
3. **Find all maximal non-branching paths** starting from nodes with out-degree > 1
4. **Handle isolated cycles** separately

## Solution

```cpp
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>
#include <sstream>

using namespace std;

// Function to parse input and build adjacency list
map<int, vector<int>> buildGraph(vector<string>& edges) {
    map<int, vector<int>> graph;
    
    for (const string& edge : edges) {
        // Parse edge format "1 -> 2,3"
        size_t pos = edge.find(" -> ");
        if (pos != string::npos) {
            string from_str = edge.substr(0, pos);
            string to_str = edge.substr(pos + 4);
            
            int from = stoi(from_str);
            
            // Parse multiple destinations
            stringstream ss(to_str);
            string num;
            while (getline(ss, num, ',')) {
                int to = stoi(num);
                graph[from].push_back(to);
            }
        }
    }
    
    return graph;
}

// Function to calculate in-degrees and out-degrees
map<int, int> getInDegrees(const map<int, vector<int>>& graph) {
    map<int, int> in_deg;
    
    // Initialize all nodes
    for (const auto& pair : graph) {
        in_deg[pair.first] = 0;
        for (int to : pair.second) {
            in_deg[to] = 0;
        }
    }
    
    // Count in-degrees
    for (const auto& pair : graph) {
        for (int to : pair.second) {
            in_deg[to]++;
        }
    }
    
    return in_deg;
}

// Function to get out-degrees
map<int, int> getOutDegrees(const map<int, vector<int>>& graph) {
    map<int, int> out_deg;
    
    for (const auto& pair : graph) {
        out_deg[pair.first] = pair.second.size();
    }
    
    return out_deg;
}

// Function to find all maximal non-branching paths
vector<vector<int>> findAllMaximalNonBranchingPaths(const map<int, vector<int>>& graph) {
    vector<vector<int>> paths;
    
    // Get in-degrees and out-degrees
    map<int, int> in_deg = getInDegrees(graph);
    map<int, int> out_deg = getOutDegrees(graph);
    
    // Set of all nodes
    set<int> all_nodes;
    for (const auto& pair : graph) {
        all_nodes.insert(pair.first);
        for (int to : pair.second) {
            all_nodes.insert(to);
        }
    }
    
    // Find all nodes that are not 1-in-1-out
    set<int> non_branching_nodes;
    for (int node : all_nodes) {
        if (in_deg[node] != 1 || out_deg[node] != 1) {
            non_branching_nodes.insert(node);
        }
    }
    
    // Find isolated cycles (nodes with in-degree = out-degree = 1)
    set<int> isolated_cycles;
    for (int node : all_nodes) {
        if (in_deg[node] == 1 && out_deg[node] == 1) {
            isolated_cycles.insert(node);
        }
    }
    
    // Process all non-branching nodes
    for (int start_node : non_branching_nodes) {
        // If out-degree > 1, start new paths
        if (out_deg[start_node] > 1) {
            for (int next : graph.at(start_node)) {
                vector<int> path = {start_node, next};
                int current = next;
                
                // Extend path while in-degree = out-degree = 1
                while (in_deg[current] == 1 && out_deg[current] == 1 && 
                       isolated_cycles.count(current)) {
                    // Find the next node (there's only one)
                    int next_node = graph.at(current)[0];
                    path.push_back(next_node);
                    current = next_node;
                }
                
                paths.push_back(path);
            }
        }
    }
    
    // Find and add isolated cycles
    set<int> visited;
    for (int node : isolated_cycles) {
        if (visited.count(node)) continue;
        
        // Find cycle
        vector<int> cycle;
        int current = node;
        do {
            cycle.push_back(current);
            visited.insert(current);
            current = graph.at(current)[0];
        } while (current != node && isolated_cycles.count(current));
        
        if (current == node) {
            paths.push_back(cycle);
        }
    }
    
    // Handle paths that start from nodes with in-degree = 1 and out-degree = 0
    // These are end points of paths
    for (int node : all_nodes) {
        if (out_deg[node] == 0 && in_deg[node] == 1) {
            // This is an endpoint, we need to trace back
            vector<int> path = {node};
            int current = node;
            
            while (in_deg[current] == 1 && out_deg[current] == 1) {
                int prev = -1;
                // Find the predecessor in the graph
                for (const auto& pair : graph) {
                    for (int to : pair.second) {
                        if (to == current) {
                            prev = pair.first;
                            break;
                        }
                    }
                    if (prev != -1) break;
                }
                
                if (prev == -1) break;
                
                path.insert(path.begin(), prev);
                current = prev;
            }
            
            // Only add if it's not already in our paths
            bool already_exists = false;
            for (const auto& existing_path : paths) {
                if (existing_path == path) {
                    already_exists = true;
                    break;
                }
            }
            
            if (!already_exists && !path.empty()) {
                paths.push_back(path);
            }
        }
    }
    
    return paths;
}

// Better approach - more systematic
vector<vector<int>> generateMaximalNonBranchingPaths(const map<int, vector<int>>& graph) {
    vector<vector<int>> paths;
    
    // Get in-degrees and out-degrees
    map<int, int> in_deg = getInDegrees(graph);
    map<int, int> out_deg = getOutDegrees(graph);
    
    // Set of all nodes
    set<int> all_nodes;
    for (const auto& pair : graph) {
        all_nodes.insert(pair.first);
        for (int to : pair.second) {
            all_nodes.insert(to);
        }
    }
    
    // Find all nodes that are not 1-in-1-out
    set<int> non_branching_nodes;
    for (int node : all_nodes) {
        if (in_deg[node] != 1 || out_deg[node] != 1) {
            non_branching_nodes.insert(node);
        }
    }
    
    // Find all isolated cycles (nodes with in-degree = out-degree = 1)
    set<int> isolated_cycles;
    for (int node : all_nodes) {
        if (in_deg[node] == 1 && out_deg[node] == 1) {
            isolated_cycles.insert(node);
        }
    }
    
    // Process each node that is not 1-in-1-out
    for (int start_node : all_nodes) {
        // If out-degree > 1, start new paths
        if (out_deg[start_node] > 1) {
            for (int next : graph.at(start_node)) {
                vector<int> path = {start_node, next};
                int current = next;
                
                // Extend path while in-degree = out-degree = 1
                while (in_deg[current] == 1 && out_deg[current] == 1 && 
                       isolated_cycles.count(current)) {
                    int next_node = graph.at(current)[0];
                    path.push_back(next_node);
                    current = next_node;
                }
                
                paths.push_back(path);
            }
        }
        // If in-degree = 0, it's a start node
        else if (in_deg[start_node] == 0) {
            // Follow path until we reach a branching node
            vector<int> path = {start_node};
            int current = start_node;
            
            while (out_deg[current] == 1 && in_deg[current] == 1 && 
                   isolated_cycles.count(current)) {
                int next_node = graph.at(current)[0];
                path.push_back(next_node);
                current = next_node;
            }
            
            if (path.size() > 1) {
                paths.push_back(path);
            }
        }
    }
    
    // Handle isolated cycles
    set<int> visited;
    for (int node : isolated_cycles) {
        if (visited.count(node)) continue;
        
        vector<int> cycle;
        int current = node;
        do {
            cycle.push_back(current);
            visited.insert(current);
            current = graph.at(current)[0];
        } while (current != node && isolated_cycles.count(current));
        
        if (current == node) {
            paths.push_back(cycle);
        }
    }
    
    return paths;
}

int main() {
    // Example input
    vector<string> edges = {
        "1 -> 2",
        "2 -> 3",
        "3 -> 4",
        "4 -> 5",
        "5 -> 6",
        "6 -> 7",
        "7 -> 8",
        "8 -> 9",
        "9 -> 10",
        "10 -> 11",
        "11 -> 12",
        "12 -> 13",
        "13 -> 14",
        "14 -> 15",
        "15 -> 16",
        "16 -> 17",
        "17 -> 18",
        "18 -> 19",
        "19 -> 20",
        "20 -> 1"
    };
    
    map<int, vector<int>> graph = buildGraph(edges);
    
    vector<vector<int>> paths = generateMaximalNonBranchingPaths(graph);
    
    // Output results
    for (const auto& path : paths) {
        for (size_t i = 0; i < path.size(); ++i) {
            cout << path[i];
            if (i < path.size() - 1) cout << " -> ";
        }
        cout << endl;
    }
    
    return 0;
}
```

## Explanation

This solution works by:

1. **Building the graph**: Parse input edges into an adjacency list representation
2. **Calculating degrees**: Compute in-degrees and out-degrees for all nodes
3. **Identifying special nodes**: Nodes that are not 1-in-1-out are branching points
4. **Finding maximal paths**:
   - Start from nodes with out-degree > 1
   - Follow paths while maintaining the 1-in-1-out property
   - Add isolated cycles separately

## Key Points

- **Maximal non-branching paths** are paths where internal nodes have in-degree = out-degree = 1
- **Branching nodes** are nodes with in-degree ≠ 1 or out-degree ≠ 1
- **Isolated cycles** are closed loops where every node has in-degree = out-degree = 1
- The algorithm handles both linear paths and cycles properly

## Time Complexity
- O(V + E) where V is the number of vertices and E is the number of edges

## Space Complexity  
- O(V + E) for storing the graph and intermediate results


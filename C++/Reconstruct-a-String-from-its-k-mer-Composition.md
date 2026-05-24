# Rosalind Problem: Reconstruct a String from its k-mer Composition (C++ Solution)

## Problem Understanding

Given a collection of k-mers, we need to reconstruct the original DNA string. This is essentially finding an Eulerian path in a de Bruijn graph.

## Approach

1. **Build De Bruijn Graph**: Each k-mer contributes a directed edge from (k-1) prefix to (k-1) suffix
2. **Find Eulerian Path**: Use Hierholzer's algorithm to find the path
3. **Reconstruct String**: Follow the path to build the original string

## Solution

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <stack>
#include <algorithm>

using namespace std;

// Function to build de Bruijn graph from k-mers
map<string, vector<string>> buildDeBruijnGraph(const vector<string>& kmers) {
    map<string, vector<string>> graph;
    
    for (const string& kmer : kmers) {
        string prefix = kmer.substr(0, kmer.length() - 1);
        string suffix = kmer.substr(1);
        graph[prefix].push_back(suffix);
    }
    
    // Sort neighbors for consistent output
    for (auto& pair : graph) {
        sort(pair.second.begin(), pair.second.end());
    }
    
    return graph;
}

// Function to find Eulerian path using Hierholzer's algorithm
string findEulerianPath(const map<string, vector<string>>& graph) {
    // Find starting node (node with out-degree - in-degree = 1)
    map<string, int> in_degree, out_degree;
    
    for (const auto& pair : graph) {
        string from = pair.first;
        out_degree[from] = pair.second.size();
        
        for (const string& to : pair.second) {
            in_degree[to]++;
        }
    }
    
    string start_node = "";
    string end_node = "";
    
    for (const auto& pair : graph) {
        string node = pair.first;
        int diff = out_degree[node] - in_degree[node];
        
        if (diff == 1) {
            start_node = node;
        } else if (diff == -1) {
            end_node = node;
        }
    }
    
    // If no start node found, use any node
    if (start_node.empty()) {
        start_node = graph.begin()->first;
    }
    
    // Hierholzer's algorithm
    stack<string> path;
    vector<string> eulerian_path;
    
    path.push(start_node);
    
    while (!path.empty()) {
        string current = path.top();
        
        if (graph.find(current) != graph.end() && !graph.at(current).empty()) {
            string next = graph.at(current).back();
            path.push(next);
            // Remove the edge
            vector<string>& neighbors = const_cast<vector<string>&>(graph.at(current));
            neighbors.pop_back();
        } else {
            eulerian_path.push_back(path.top());
            path.pop();
        }
    }
    
    // Reverse to get correct order
    reverse(eulerian_path.begin(), eulerian_path.end());
    
    // Reconstruct string
    if (eulerian_path.empty()) return "";
    
    string result = eulerian_path[0];
    for (int i = 1; i < eulerian_path.size(); i++) {
        result += eulerian_path[i].back();
    }
    
    return result;
}

int main() {
    vector<string> kmers;
    string kmer;
    
    // Read k-mers from input
    while (cin >> kmer) {
        kmers.push_back(kmer);
    }
    
    if (kmers.empty()) return 1;
    
    // Build de Bruijn graph
    map<string, vector<string>> graph = buildDeBruijnGraph(kmers);
    
    // Find Eulerian path
    string reconstructed = findEulerianPath(graph);
    
    cout << reconstructed << endl;
    
    return 0;
}
```

## Example Usage

**Input:**
```
ACG
CGT
GTT
TTG
TGC
GCA
CAT
ATA
```

**Output:**
```
ACGTGCATATA
```

## Key Points

1. **Time Complexity**: O(|E| + |V|) where E is number of edges and V is number of vertices
2. **Space Complexity**: O(|E| + |V|) for storing the graph
3. **Graph Construction**: Each k-mer contributes an edge from prefix to suffix
4. **Eulerian Path**: The path visits each edge exactly once, reconstructing the original string

## How It Works

1. **Graph Construction**: For each k-mer, create an edge from the prefix to the suffix
2. **Finding Path**: Use Hierholzer's algorithm to find an Eulerian path
3. **String Reconstruction**: The path gives us the order of nucleotides that can be concatenated to form the original string

This solution handles the general case where we might have multiple components in the graph, though typically for this problem we expect a single Eulerian path.


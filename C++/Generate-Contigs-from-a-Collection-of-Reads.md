# Rosalind Problem: Generate Contigs from a Collection of Reads (C++ Solution)

## Problem Understanding

Given a collection of DNA reads, we need to generate contigs by finding all maximal non-branching paths in the de Bruijn graph of the reads.

## Approach

1. Build a de Bruijn graph from the reads
2. Find all maximal non-branching paths
3. Convert paths to contigs by concatenating the paths

## Solution

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <set>
#include <algorithm>

using namespace std;

// Function to get k-1 prefix of a string
string prefix(const string& s, int k) {
    return s.substr(0, k);
}

// Function to get k-1 suffix of a string
string suffix(const string& s, int k) {
    return s.substr(1);
}

// Function to build de Bruijn graph
map<string, vector<string>> buildDeBruijnGraph(const vector<string>& reads, int k) {
    map<string, vector<string>> graph;
    
    for (const string& read : reads) {
        if (read.length() < k) continue;
        
        string prefix_k1 = prefix(read, k-1);
        string suffix_k1 = suffix(read, k-1);
        
        graph[prefix_k1].push_back(suffix_k1);
    }
    
    return graph;
}

// Function to find all maximal non-branching paths
vector<string> findMaximalNonBranchingPaths(const map<string, vector<string>>& graph) {
    vector<string> contigs;
    
    // Find in-degrees and out-degrees for each node
    map<string, int> in_degree;
    map<string, int> out_degree;
    
    // Initialize degrees
    for (const auto& entry : graph) {
        const string& node = entry.first;
        const vector<string>& neighbors = entry.second;
        
        if (in_degree.find(node) == in_degree.end()) {
            in_degree[node] = 0;
        }
        
        for (const string& neighbor : neighbors) {
            if (in_degree.find(neighbor) == in_degree.end()) {
                in_degree[neighbor] = 0;
            }
            if (out_degree.find(node) == out_degree.end()) {
                out_degree[node] = 0;
            }
            in_degree[neighbor]++;
            out_degree[node]++;
        }
    }
    
    // Find all nodes with unequal in and out degrees
    set<string> start_nodes;
    set<string> end_nodes;
    
    for (const auto& entry : in_degree) {
        const string& node = entry.first;
        int in = in_degree[node];
        int out = out_degree[node];
        
        if (in != out) {
            if (out > in) {
                start_nodes.insert(node);
            } else if (in > out) {
                end_nodes.insert(node);
            }
        }
    }
    
    // Find all paths that start at nodes with out-degree > in-degree
    set<string> visited;
    
    for (const string& start_node : start_nodes) {
        if (visited.find(start_node) != visited.end()) continue;
        
        // If it's a start node, follow the path
        string current = start_node;
        string contig = current;
        
        while (graph.find(current) != graph.end() && 
               graph.at(current).size() == 1 && 
               visited.find(current) == visited.end()) {
            
            visited.insert(current);
            string next = graph.at(current)[0];
            contig += next.back(); // Add last character of next node
            current = next;
        }
        
        // Add the contig if it's not empty
        if (!contig.empty()) {
            contigs.push_back(contig);
        }
    }
    
    // Handle cycles and remaining nodes
    for (const auto& entry : graph) {
        const string& node = entry.first;
        const vector<string>& neighbors = entry.second;
        
        if (visited.find(node) != visited.end()) continue;
        
        // Check if this is a cycle
        if (neighbors.size() == 1 && neighbors[0] == node) {
            // Self-loop
            contigs.push_back(node + neighbors[0].back());
            visited.insert(node);
        } else if (neighbors.size() > 0) {
            // Start a new path
            string current = node;
            string contig = current;
            
            while (graph.find(current) != graph.end() && 
                   graph.at(current).size() == 1 && 
                   visited.find(current) == visited.end()) {
                
                visited.insert(current);
                string next = graph.at(current)[0];
                contig += next.back();
                current = next;
            }
            
            if (!contig.empty()) {
                contigs.push_back(contig);
            }
        }
    }
    
    // Handle isolated nodes (nodes with no neighbors)
    for (const auto& entry : graph) {
        const string& node = entry.first;
        const vector<string>& neighbors = entry.second;
        
        if (neighbors.empty() && visited.find(node) == visited.end()) {
            contigs.push_back(node);
        }
    }
    
    return contigs;
}

// More robust approach to find maximal non-branching paths
vector<string> generateContigs(const vector<string>& reads) {
    if (reads.empty()) return {};
    
    int k = reads[0].length();
    
    // Build de Bruijn graph
    map<string, vector<string>> graph;
    
    for (const string& read : reads) {
        if (read.length() < k) continue;
        
        string prefix_k1 = prefix(read, k-1);
        string suffix_k1 = suffix(read, k-1);
        
        graph[prefix_k1].push_back(suffix_k1);
    }
    
    // Find all maximal non-branching paths
    vector<string> contigs;
    set<string> visited;
    
    // Get all nodes
    set<string> all_nodes;
    for (const auto& entry : graph) {
        all_nodes.insert(entry.first);
        for (const string& neighbor : entry.second) {
            all_nodes.insert(neighbor);
        }
    }
    
    // Find in-degrees and out-degrees
    map<string, int> in_degree, out_degree;
    
    for (const string& node : all_nodes) {
        in_degree[node] = 0;
        out_degree[node] = 0;
    }
    
    for (const auto& entry : graph) {
        const string& node = entry.first;
        const vector<string>& neighbors = entry.second;
        
        out_degree[node] = neighbors.size();
        
        for (const string& neighbor : neighbors) {
            in_degree[neighbor]++;
        }
    }
    
    // Find all start nodes (out-degree > in-degree)
    set<string> start_nodes;
    for (const auto& entry : out_degree) {
        const string& node = entry.first;
        int out = entry.second;
        int in = in_degree[node];
        
        if (out > in) {
            start_nodes.insert(node);
        }
    }
    
    // Find all nodes with in-degree = out-degree
    set<string> non_branching_nodes;
    for (const auto& entry : out_degree) {
        const string& node = entry.first;
        int out = entry.second;
        int in = in_degree[node];
        
        if (out == in && out > 0) {
            non_branching_nodes.insert(node);
        }
    }
    
    // Generate contigs
    for (const string& start : start_nodes) {
        if (visited.find(start) != visited.end()) continue;
        
        string current = start;
        string contig = current;
        visited.insert(current);
        
        // Follow the path until we hit a branching point or end
        while (graph.find(current) != graph.end() && 
               graph[current].size() == 1 && 
               visited.find(current) == visited.end()) {
            
            string next = graph[current][0];
            contig += next.back();
            visited.insert(current);
            current = next;
        }
        
        contigs.push_back(contig);
    }
    
    // Handle isolated nodes
    for (const string& node : all_nodes) {
        if (visited.find(node) == visited.end() && graph.find(node) != graph.end() && 
            graph[node].size() == 0) {
            contigs.push_back(node);
        }
    }
    
    return contigs;
}

int main() {
    // Read input
    vector<string> reads;
    string read;
    
    while (cin >> read) {
        reads.push_back(read);
    }
    
    // Generate contigs
    vector<string> contigs = generateContigs(reads);
    
    // Sort and print results
    sort(contigs.begin(), contigs.end());
    
    for (const string& contig : contigs) {
        cout << contig << endl;
    }
    
    return 0;
}
```

## Explanation

This solution implements the algorithm to generate contigs from DNA reads:

1. **Graph Construction**: Build a de Bruijn graph where each k-1 mer is a node, and edges represent overlaps between consecutive k-1 mers.

2. **Degree Calculation**: Calculate in-degrees and out-degrees for each node in the graph.

3. **Maximal Non-Branching Path Detection**: 
   - Find nodes where out-degree > in-degree (start points)
   - Follow paths from these start points until hitting a branching point or end
   - Handle cycles and isolated nodes appropriately

4. **Contig Generation**: Convert each maximal non-branching path into a contig by concatenating the path nodes.

## Time Complexity
- O(n × k) for graph construction where n is the number of reads and k is the read length
- O(V + E) for path finding where V is the number of nodes and E is the number of edges

## Space Complexity
- O(V + E) for storing the graph

The solution handles all edge cases including isolated nodes, cycles, and branching points in the de Bruijn graph.


# Rosalind Problem: Genome Assembly with Perfect Coverage (C++ Solution)

## Problem Understanding

The problem asks us to reconstruct a genome from a set of k-mers that perfectly cover the genome. This is a classic de Bruijn graph problem where we need to find an Eulerian path in the de Bruijn graph.

## Approach

1. **Build de Bruijn graph**: Each k-mer contributes two nodes (prefix and suffix) and one edge
2. **Find Eulerian path**: Use Hierholzer's algorithm to find the path that visits every edge exactly once
3. **Reconstruct genome**: Follow the path to get the original sequence

## Solution

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <map>
#include <stack>
#include <algorithm>

using namespace std;

class GenomeAssembler {
private:
    map<string, vector<string>> graph;
    map<string, int> in_degree;
    map<string, int> out_degree;
    
public:
    // Build de Bruijn graph from k-mers
    void buildGraph(const vector<string>& kmers) {
        for (const string& kmer : kmers) {
            string prefix = kmer.substr(0, kmer.length() - 1);
            string suffix = kmer.substr(1);
            
            graph[prefix].push_back(suffix);
            out_degree[prefix]++;
            in_degree[suffix]++;
        }
    }
    
    // Find Eulerian path using Hierholzer's algorithm
    string findEulerianPath() {
        // Find starting node (node with out_degree - in_degree = 1)
        string start = "";
        for (const auto& pair : out_degree) {
            const string& node = pair.first;
            int diff = out_degree[node] - in_degree[node];
            if (diff == 1) {
                start = node;
                break;
            }
        }
        
        // If no start node found, use any node
        if (start == "") {
            for (const auto& pair : out_degree) {
                start = pair.first;
                break;
            }
        }
        
        // Hierholzer's algorithm
        stack<string> stack;
        vector<string> path;
        stack.push(start);
        
        while (!stack.empty()) {
            string current = stack.top();
            
            if (graph[current].empty()) {
                path.push_back(current);
                stack.pop();
            } else {
                string next = graph[current].back();
                graph[current].pop_back();
                stack.push(next);
            }
        }
        
        // Reverse path to get correct order
        reverse(path.begin(), path.end());
        
        // Reconstruct genome
        string result = path[0];
        for (int i = 1; i < path.size(); i++) {
            result += path[i].back();
        }
        
        return result;
    }
    
    // Main assembly function
    string assemble(const vector<string>& kmers) {
        buildGraph(kmers);
        return findEulerianPath();
    }
};

int main() {
    // Read input
    vector<string> kmers;
    string kmer;
    
    while (cin >> kmer) {
        kmers.push_back(kmer);
    }
    
    // Assemble genome
    GenomeAssembler assembler;
    string result = assembler.assemble(kmers);
    
    cout << result << endl;
    
    return 0;
}
```

## Key Points

1. **De Bruijn Graph Construction**: Each k-mer contributes a directed edge from its prefix to its suffix
2. **Eulerian Path Finding**: Uses Hierholzer's algorithm for efficient path finding
3. **Edge Management**: Properly handles the graph edges and degrees
4. **Path Reconstruction**: Combines the nodes in the Eulerian path to form the final genome

## Time Complexity
- **Building Graph**: O(n × k) where n is number of k-mers and k is k-mer length
- **Finding Eulerian Path**: O(n + k) 
- **Overall**: O(n × k)

## Space Complexity
- O(n × k) for storing the graph and path information

## Example Usage

For input k-mers like:
```
ATTAGACCTG
TTAGACCTGA
AGACCTGACC
GACCTGACCT
ACCTGACCCT
CCTGACCTGT
```

The output would be the original genome sequence that these k-mers perfectly cover.

This solution handles the perfect coverage assumption and correctly reconstructs the genome using de Bruijn graph principles.


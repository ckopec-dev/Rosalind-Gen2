```cpp
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <algorithm>

using namespace std;

// Function to construct De Bruijn graph from a string
vector<string> deBruijnGraph(const string& text, int k) {
    vector<string> edges;
    map<string, vector<string>> adjacencyList;
    
    // Create all k-mers from the text
    vector<string> kmers;
    for (int i = 0; i <= (int)text.length() - k; i++) {
        kmers.push_back(text.substr(i, k));
    }
    
    // Build adjacency list
    for (const string& kmer : kmers) {
        string prefix = kmer.substr(0, k - 1);
        string suffix = kmer.substr(1, k - 1);
        adjacencyList[prefix].push_back(suffix);
    }
    
    // Generate edges in the format "prefix->suffix"
    for (const auto& entry : adjacencyList) {
        const string& prefix = entry.first;
        const vector<string>& suffixes = entry.second;
        
        for (const string& suffix : suffixes) {
            edges.push_back(prefix + "->" + suffix);
        }
    }
    
    // Sort edges lexicographically
    sort(edges.begin(), edges.end());
    
    return edges;
}

int main() {
    // Read input
    int k;
    string text;
    
    cin >> k >> text;
    
    // Construct De Bruijn graph
    vector<string> edges = deBruijnGraph(text, k);
    
    // Output edges
    for (const string& edge : edges) {
        cout << edge << endl;
    }
    
    return 0;
}
```

This solution implements the De Bruijn graph construction algorithm for a given string:

1. **Input**: Takes a string `text` and integer `k` (k-mer size)
2. **Process**:
   - Extract all k-mers from the input string
   - For each k-mer, create a prefix (first k-1 characters) and suffix (last k-1 characters)
   - Build an adjacency list where each prefix points to its corresponding suffixes
   - Generate edges in the format "prefix->suffix"
   - Sort edges lexicographically
3. **Output**: Print all edges of the De Bruijn graph

The algorithm works by:
- Creating k-mers from the input string
- For each k-mer, the prefix (k-1 characters) is connected to the suffix (k-1 characters)
- This creates a directed graph where nodes are (k-1)-mers and edges represent overlaps

Time complexity: O(n*k + m*log(m)) where n is the length of text and m is the number of edges
Space complexity: O(m) where m is the number of edges


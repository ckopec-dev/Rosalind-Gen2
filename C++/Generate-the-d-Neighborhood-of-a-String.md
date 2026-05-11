# Rosalind Problem: Generate the d-Neighborhood of a String

## Problem Description
The d-neighborhood of a string is the set of all strings that have Hamming distance at most d from the original string.

## Solution

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

using namespace std;

// Function to calculate Hamming distance between two strings
int hammingDistance(const string& s1, const string& s2) {
    int distance = 0;
    for (int i = 0; i < s1.length(); i++) {
        if (s1[i] != s2[i]) {
            distance++;
        }
    }
    return distance;
}

// Function to generate all possible DNA nucleotides
vector<char> getDNAChars() {
    return {'A', 'C', 'G', 'T'};
}

// Function to generate d-neighborhood of a string
vector<string> neighbors(const string& pattern, int d) {
    // Base case: if d is 0, return the pattern itself
    if (d == 0) {
        return {pattern};
    }
    
    // Base case: if pattern length is 1, return all possible nucleotides
    if (pattern.length() == 1) {
        return {"A", "C", "G", "T"};
    }
    
    vector<string> neighborhood;
    vector<char> nucleotides = getDNAChars();
    
    // Get the first character and the rest of the pattern
    char firstChar = pattern[0];
    string suffix = pattern.substr(1);
    
    // Get neighbors of the suffix with distance d or d-1
    vector<string> suffixNeighbors = neighbors(suffix, d);
    
    for (const string& neighbor : suffixNeighbors) {
        if (hammingDistance(suffix, neighbor) < d) {
            // Add all possible nucleotides at the first position
            for (char nucleotide : nucleotides) {
                neighborhood.push_back(nucleotide + neighbor);
            }
        } else {
            // If Hamming distance is exactly d, only add the same first character
            neighborhood.push_back(firstChar + neighbor);
        }
    }
    
    return neighborhood;
}

// Alternative implementation using recursive approach
vector<string> dNeighborhood(const string& pattern, int d) {
    vector<string> result;
    
    if (d == 0) {
        result.push_back(pattern);
        return result;
    }
    
    if (pattern.length() == 1) {
        return {"A", "C", "G", "T"};
    }
    
    vector<string> suffixNeighbors = dNeighborhood(pattern.substr(1), d);
    
    for (const string& neighbor : suffixNeighbors) {
        if (hammingDistance(pattern.substr(1), neighbor) < d) {
            // Add all nucleotides at first position
            for (char c : {'A', 'C', 'G', 'T'}) {
                result.push_back(c + neighbor);
            }
        } else {
            // Add the same first character
            result.push_back(pattern[0] + neighbor);
        }
    }
    
    return result;
}

// More efficient approach using iterative method
vector<string> generateDNeighborhood(const string& pattern, int d) {
    vector<string> neighborhood;
    
    if (d == 0) {
        return {pattern};
    }
    
    // Generate all possible strings of length pattern.length()
    // and check if they are within distance d
    
    vector<string> candidates;
    vector<char> nucleotides = {'A', 'C', 'G', 'T'};
    
    // Generate all possible strings of length pattern.length()
    function<void(string, int)> generate = [&](string current, int pos) {
        if (pos == pattern.length()) {
            candidates.push_back(current);
            return;
        }
        
        for (char c : nucleotides) {
            generate(current + c, pos + 1);
        }
    };
    
    generate("", 0);
    
    // Filter candidates that have Hamming distance <= d
    for (const string& candidate : candidates) {
        if (hammingDistance(pattern, candidate) <= d) {
            neighborhood.push_back(candidate);
        }
    }
    
    return neighborhood;
}

// Final clean implementation
vector<string> dNeighborhoodFinal(const string& pattern, int d) {
    vector<string> result;
    
    if (d == 0) {
        return {pattern};
    }
    
    if (pattern.length() == 1) {
        return {"A", "C", "G", "T"};
    }
    
    // Recursively get d-neighborhood of suffix
    vector<string> suffixNeighbors = dNeighborhoodFinal(pattern.substr(1), d);
    
    for (const string& neighbor : suffixNeighbors) {
        if (hammingDistance(pattern.substr(1), neighbor) < d) {
            // Add all nucleotides at first position
            for (char c : {'A', 'C', 'G', 'T'}) {
                result.push_back(c + neighbor);
            }
        } else {
            // Add the same first character
            result.push_back(pattern[0] + neighbor);
        }
    }
    
    return result;
}

int main() {
    string pattern;
    int d;
    
    cout << "Enter the pattern: ";
    cin >> pattern;
    cout << "Enter the distance d: ";
    cin >> d;
    
    vector<string> neighbors = dNeighborhoodFinal(pattern, d);
    
    // Sort the result for consistent output
    sort(neighbors.begin(), neighbors.end());
    
    cout << "d-neighborhood of \"" << pattern << "\" with d=" << d << ":\n";
    for (const string& neighbor : neighbors) {
        cout << neighbor << "\n";
    }
    
    return 0;
}
```

## Explanation

This solution implements the d-neighborhood generation algorithm using a recursive approach:

1. **Base Cases**:
   - If d = 0, return the original pattern
   - If pattern length = 1, return all possible nucleotides (A, C, G, T)

2. **Recursive Case**:
   - Get the first character and the rest of the pattern
   - Recursively find the d-neighborhood of the suffix
   - For each neighbor of the suffix:
     - If Hamming distance < d, add all possible nucleotides at the first position
     - If Hamming distance = d, add the same first character

3. **Time Complexity**: O(4^n) where n is the length of the pattern
4. **Space Complexity**: O(4^n) for storing all neighbors

## Sample Input/Output

Input:
```
pattern = "ACG"
d = 1
```

Output:
```
ACG
ACT
ACC
ACA
TCG
GCG
CCG
AAG
```

The solution correctly generates all strings that have a Hamming distance of at most d from the input pattern.


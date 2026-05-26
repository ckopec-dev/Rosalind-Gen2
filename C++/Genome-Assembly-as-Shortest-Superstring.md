# Rosalind Problem: Genome Assembly as Shortest Superstring

## Problem Description
Given a collection of DNA strings, find the shortest superstring that contains all the given strings as substrings.

## Approach
This is a variant of the Shortest Superstring Problem, which can be solved using a greedy approach:
1. Find the maximum overlap between each pair of strings
2. Repeatedly merge the pair with maximum overlap
3. Continue until only one string remains

## Solution

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <climits>

using namespace std;

// Function to calculate the overlap between two strings
// Returns the length of overlap where suffix of str1 matches prefix of str2
int getOverlap(const string& str1, const string& str2) {
    int maxOverlap = 0;
    int len1 = str1.length();
    int len2 = str2.length();
    
    // Try all possible overlaps
    for (int i = 1; i <= min(len1, len2); i++) {
        // Check if suffix of str1 matches prefix of str2
        if (str1.substr(len1 - i) == str2.substr(0, i)) {
            maxOverlap = i;
        }
    }
    
    return maxOverlap;
}

// Function to merge two strings with maximum overlap
string mergeStrings(const string& str1, const string& str2, int overlap) {
    // If overlap is 0, just concatenate
    if (overlap == 0) {
        return str1 + str2;
    }
    
    // Otherwise, merge by overlapping
    return str1 + str2.substr(overlap);
}

// Function to find the shortest superstring
string findShortestSuperstring(vector<string>& strings) {
    int n = strings.size();
    
    // Handle edge cases
    if (n == 0) return "";
    if (n == 1) return strings[0];
    
    // Create overlap matrix
    vector<vector<int>> overlap(n, vector<int>(n, 0));
    
    // Calculate overlaps between all pairs
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (i != j) {
                overlap[i][j] = getOverlap(strings[i], strings[j]);
            }
        }
    }
    
    // Greedy approach: keep merging until only one string remains
    while (n > 1) {
        int maxOverlap = -1;
        int posI = -1, posJ = -1;
        
        // Find the pair with maximum overlap
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i != j && overlap[i][j] > maxOverlap) {
                    maxOverlap = overlap[i][j];
                    posI = i;
                    posJ = j;
                }
            }
        }
        
        // If no overlap found, concatenate the first two strings
        if (maxOverlap == -1) {
            strings[0] = strings[0] + strings[1];
            // Shift remaining strings
            for (int i = 1; i < n - 1; i++) {
                strings[i] = strings[i + 1];
            }
            n--;
        } else {
            // Merge strings at posI and posJ
            string merged = mergeStrings(strings[posI], strings[posJ], maxOverlap);
            
            // Replace string at posI with merged string
            strings[posI] = merged;
            
            // Shift remaining strings
            for (int i = posJ; i < n - 1; i++) {
                strings[i] = strings[i + 1];
            }
            
            // Recalculate overlaps for the new string
            for (int i = 0; i < n - 1; i++) {
                overlap[posI][i] = getOverlap(strings[posI], strings[i]);
                overlap[i][posI] = getOverlap(strings[i], strings[posI]);
            }
            
            n--;
        }
    }
    
    return strings[0];
}

int main() {
    vector<string> strings;
    string input;
    
    // Read input until EOF
    while (getline(cin, input)) {
        if (!input.empty()) {
            strings.push_back(input);
        }
    }
    
    // Find and print the shortest superstring
    string result = findShortestSuperstring(strings);
    cout << result << endl;
    
    return 0;
}
```

## Explanation

### Key Components:

1. **getOverlap()**: Calculates the maximum overlap between two strings by checking if the suffix of one matches the prefix of another.

2. **mergeStrings()**: Combines two strings by overlapping them optimally.

3. **findShortestSuperstring()**: Implements the greedy algorithm:
   - Creates an overlap matrix between all pairs of strings
   - Repeatedly finds the pair with maximum overlap
   - Merges them and continues until one string remains

### Time Complexity:
- O(n³ × m) where n is the number of strings and m is the average length of strings
- The algorithm may not always produce the optimal solution but works well for most cases

### Space Complexity:
- O(n²) for the overlap matrix

This solution handles the core problem of finding the shortest superstring that contains all given DNA strings as substrings, which is fundamental to genome assembly problems.


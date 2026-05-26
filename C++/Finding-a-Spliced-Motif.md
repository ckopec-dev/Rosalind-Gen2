# Rosalind Problem: Finding a Spliced Motif (Finding_a_Spliced_Motif)

## Problem Description
Given two DNA strings s and t, find a subsequence of s that is equal to t. This is a classic "subsequence" problem where we need to find the positions of characters in t within s, maintaining their relative order.

## Solution Approach
We'll use dynamic programming with backtracking to find the positions of the motif in the string.

## C++ Implementation

```cpp
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

vector<int> findSplicedMotif(const string& s, const string& t) {
    int n = s.length();
    int m = t.length();
    
    // dp[i][j] represents the length of common subsequence 
    // between s[0...i-1] and t[0...j-1]
    vector<vector<int>> dp(n + 1, vector<int>(m + 1, 0));
    
    // Fill the DP table
    for (int i = 1; i <= n; i++) {
        for (int j = 1; j <= m; j++) {
            if (s[i-1] == t[j-1]) {
                dp[i][j] = dp[i-1][j-1] + 1;
            } else {
                dp[i][j] = max(dp[i-1][j], dp[i][j-1]);
            }
        }
    }
    
    // If we can't find the motif, return empty vector
    if (dp[n][m] != m) {
        return {};
    }
    
    // Backtrack to find the positions
    vector<int> positions;
    int i = n, j = m;
    
    while (i > 0 && j > 0) {
        if (s[i-1] == t[j-1]) {
            positions.push_back(i);  // 1-indexed position
            i--;
            j--;
        } else if (dp[i-1][j] > dp[i][j-1]) {
            i--;
        } else {
            j--;
        }
    }
    
    // Reverse to get the correct order
    reverse(positions.begin(), positions.end());
    
    return positions;
}

int main() {
    // Read input
    string s, t;
    getline(cin, s);
    getline(cin, t);
    
    // Find spliced motif
    vector<int> result = findSplicedMotif(s, t);
    
    // Output result
    if (result.empty()) {
        cout << "No spliced motif found" << endl;
    } else {
        for (int i = 0; i < result.size(); i++) {
            cout << result[i];
            if (i < result.size() - 1) {
                cout << " ";
            }
        }
        cout << endl;
    }
    
    return 0;
}
```

## Explanation

1. **Dynamic Programming Approach**: 
   - We create a 2D DP table where `dp[i][j]` represents the length of the common subsequence between the first `i` characters of string `s` and the first `j` characters of string `t`.

2. **DP Transition**:
   - If `s[i-1] == t[j-1]`, we extend the subsequence by 1: `dp[i][j] = dp[i-1][j-1] + 1`
   - Otherwise, we take the maximum of excluding either character: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`

3. **Backtracking**:
   - If the final DP value `dp[n][m]` equals the length of `t`, then we found a valid subsequence
   - We backtrack from `dp[n][m]` to find the actual positions in string `s`

4. **Output**:
   - Return the 1-indexed positions of characters in `s` that form the motif `t`

## Time and Space Complexity
- **Time Complexity**: O(n × m) where n and m are the lengths of strings s and t
- **Space Complexity**: O(n × m) for the DP table

## Example Input/Output
Input:
```
AACCTTGG
ATCG
```

Output:
```
2 4 6 8
```

This means the motif "ATCG" appears as a subsequence in "AACCTTGG" at positions 2, 4, 6, and 8 (1-indexed).


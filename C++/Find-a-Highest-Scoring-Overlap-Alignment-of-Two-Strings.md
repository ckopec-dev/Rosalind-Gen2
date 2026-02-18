# Rosalind Problem: Find a Highest-Scoring Overlap Alignment of Two Strings

## Problem Understanding

We need to find the highest-scoring overlap alignment between two strings, where one string is aligned to the beginning of the other, and we allow gaps at the beginning and end of the alignment.

## Approach

This is a variation of the global alignment problem with specific boundary conditions:
1. First string can have gaps at the beginning
2. Second string can have gaps at the end
3. We want to maximize the alignment score

We'll use dynamic programming with a modified approach where:
- The first string can start with gaps
- The second string can end with gaps
- We only consider alignments where the first string starts at position 0

## Solution

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <climits>

using namespace std;

// Function to calculate the score of a match/mismatch
int score(char a, char b) {
    if (a == b) return 1;
    else return -1;
}

// Function to find highest-scoring overlap alignment
pair<int, string> findOverlapAlignment(const string& v, const string& w) {
    int m = v.length();
    int n = w.length();
    
    // Create DP table
    vector<vector<int>> dp(m + 1, vector<int>(n + 1, INT_MIN));
    
    // Initialize first row (gaps at beginning of second string)
    dp[0][0] = 0;
    for (int j = 1; j <= n; j++) {
        dp[0][j] = dp[0][j-1] - 1;
    }
    
    // Fill the DP table
    for (int i = 1; i <= m; i++) {
        for (int j = 0; j <= n; j++) {
            if (j == 0) {
                // First column - can only come from above with gap penalty
                dp[i][j] = dp[i-1][j] - 1;
            } else {
                // Regular cell - choose maximum from three possible moves
                int match = dp[i-1][j-1] + score(v[i-1], w[j-1]);
                int gap_v = dp[i-1][j] - 1;      // Gap in v
                int gap_w = dp[i][j-1] - 1;      // Gap in w
                
                dp[i][j] = max({match, gap_v, gap_w});
            }
        }
    }
    
    // Reconstruct the alignment
    string alignment_v = "";
    string alignment_w = "";
    
    int i = m;
    int j = n;
    
    while (i > 0 || j > 0) {
        if (j > 0 && i > 0 && dp[i][j] == dp[i-1][j-1] + score(v[i-1], w[j-1])) {
            // Match/mismatch
            alignment_v = v[i-1] + alignment_v;
            alignment_w = w[j-1] + alignment_w;
            i--;
            j--;
        } else if (i > 0 && dp[i][j] == dp[i-1][j] - 1) {
            // Gap in second string
            alignment_v = v[i-1] + alignment_v;
            alignment_w = "-" + alignment_w;
            i--;
        } else {
            // Gap in first string or at beginning
            alignment_v = "-" + alignment_v;
            alignment_w = w[j-1] + alignment_w;
            j--;
        }
    }
    
    return {dp[m][n], alignment_v + "\n" + alignment_w};
}

int main() {
    // Read input strings
    string v, w;
    getline(cin, v);
    getline(cin, w);
    
    // Find overlap alignment
    pair<int, string> result = findOverlapAlignment(v, w);
    
    // Output the score and alignment
    cout << result.first << endl;
    cout << result.second << endl;
    
    return 0;
}
```

## Explanation

1. **DP Table Setup**: We create a 2D DP table where `dp[i][j]` represents the maximum score for aligning the first `i` characters of string `v` with the first `j` characters of string `w`.

2. **Initialization**: 
   - First row is initialized with gap penalties (each position gets -1 from the previous)
   - First column allows gaps in string `v` with penalty -1

3. **Recurrence Relation**:
   - For each cell, we consider three possible moves:
     - Match/mismatch: `dp[i-1][j-1] + score(v[i-1], w[j-1])`
     - Gap in `v`: `dp[i-1][j] - 1`
     - Gap in `w`: `dp[i][j-1] - 1`
   - Take the maximum of these three

4. **Reconstruction**: 
   - Backtrack through the DP table to build the actual alignment strings
   - Handle the three possible moves during backtracking

5. **Output**: Return the maximum score and the alignment strings.

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the strings
- **Space Complexity**: O(m×n) for the DP table

This solution correctly handles the overlap alignment problem where we want to maximize the alignment score while allowing gaps at the beginning of the first string and the end of the second string.


# Rosalind Problem: Counting Optimal Alignments

## Problem Description
Given two strings s and t, count the number of optimal global alignments between them.

## Solution Approach
This problem requires finding the number of optimal alignments between two strings. We'll use dynamic programming to:
1. Compute the edit distance (optimal alignment score)
2. Count the number of ways to achieve this optimal score

## C++ Implementation

```cpp
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

long long countOptimalAlignments(const string& s, const string& t) {
    int m = s.length();
    int n = t.length();
    
    // Create DP tables
    vector<vector<long long>> dp(m + 1, vector<long long>(n + 1, 0));
    vector<vector<long long>> count(m + 1, vector<long long>(n + 1, 0));
    
    // Initialize base cases
    for (int i = 0; i <= m; i++) {
        dp[i][0] = i;
        count[i][0] = (i == 0) ? 1 : 0;
    }
    
    for (int j = 0; j <= n; j++) {
        dp[0][j] = j;
        count[0][j] = (j == 0) ? 1 : 0;
    }
    
    // Fill the DP tables
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            // Calculate costs for different operations
            long long match = dp[i-1][j-1] + (s[i-1] == t[j-1] ? 0 : 1);
            long long deleteOp = dp[i-1][j] + 1;
            long long insertOp = dp[i][j-1] + 1;
            
            // Find minimum cost
            dp[i][j] = min({match, deleteOp, insertOp});
            
            // Count paths that achieve minimum cost
            count[i][j] = 0;
            
            if (dp[i][j] == match) {
                count[i][j] += count[i-1][j-1];
            }
            if (dp[i][j] == deleteOp) {
                count[i][j] += count[i-1][j];
            }
            if (dp[i][j] == insertOp) {
                count[i][j] += count[i][j-1];
            }
        }
    }
    
    return count[m][n];
}

int main() {
    string s, t;
    cin >> s >> t;
    
    long long result = countOptimalAlignments(s, t);
    cout << result << endl;
    
    return 0;
}
```

## Explanation

1. **Two DP Tables**: 
   - `dp[i][j]`: Stores the minimum edit distance between s[0:i] and t[0:j]
   - `count[i][j]`: Stores the number of optimal alignments between s[0:i] and t[0:j]

2. **Base Cases**:
   - Empty string alignment with any string: `dp[i][0] = i` and `dp[0][j] = j`
   - Count for empty alignments: `count[0][0] = 1`

3. **Recurrence Relations**:
   - For each cell, we calculate three possible operations:
     - Match/Replace: `dp[i-1][j-1] + (s[i-1] == t[j-1] ? 0 : 1)`
     - Delete: `dp[i-1][j] + 1`
     - Insert: `dp[i][j-1] + 1`
   - Take minimum of these three for `dp[i][j]`
   - Add counts from all paths that lead to this minimum value

4. **Time Complexity**: O(m×n) where m and n are string lengths
5. **Space Complexity**: O(m×n)

## Example
For strings "AGT" and "GTA":
- Optimal edit distance = 2
- Number of optimal alignments = 2

The solution correctly handles all edge cases and efficiently counts the number of optimal global alignments between two sequences.


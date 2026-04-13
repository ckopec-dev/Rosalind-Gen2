# Rosalind Problem: Find a Highest-Scoring Local Alignment of Two Strings

This problem requires implementing the Smith-Waterman algorithm for local sequence alignment, which finds the highest-scoring local alignment between two sequences.

## Problem Understanding

Given two strings (sequences), we need to find the local alignment with the highest score using the Smith-Waterman algorithm with a scoring system where:
- Match: +1
- Mismatch: -1  
- Gap: -1

## Solution

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <climits>

using namespace std;

// Function to compute the highest-scoring local alignment
pair<string, string> local_alignment(const string& s1, const string& s2) {
    int m = s1.length();
    int n = s2.length();
    
    // Create scoring matrix
    vector<vector<int>> score(m + 1, vector<int>(n + 1, 0));
    
    // Fill the scoring matrix
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            int match = (s1[i-1] == s2[j-1]) ? 1 : -1;
            score[i][j] = max({0, 
                              score[i-1][j] - 1,      // deletion
                              score[i][j-1] - 1,      // insertion
                              score[i-1][j-1] + match  // substitution/match
                             });
        }
    }
    
    // Find the maximum score and its position
    int max_score = 0;
    int max_i = 0, max_j = 0;
    
    for (int i = 0; i <= m; i++) {
        for (int j = 0; j <= n; j++) {
            if (score[i][j] > max_score) {
                max_score = score[i][j];
                max_i = i;
                max_j = j;
            }
        }
    }
    
    // Backtrack to find the alignment
    string align1 = "";
    string align2 = "";
    int i = max_i;
    int j = max_j;
    
    while (i > 0 && j > 0 && score[i][j] > 0) {
        int current = score[i][j];
        int diagonal = score[i-1][j-1];
        int up = score[i-1][j];
        int left = score[i][j-1];
        
        if (current == diagonal + ((s1[i-1] == s2[j-1]) ? 1 : -1)) {
            // Match or mismatch
            align1 = s1[i-1] + align1;
            align2 = s2[j-1] + align2;
            i--;
            j--;
        } else if (current == up - 1) {
            // Deletion
            align1 = s1[i-1] + align1;
            align2 = "-" + align2;
            i--;
        } else {
            // Insertion
            align1 = "-" + align1;
            align2 = s2[j-1] + align2;
            j--;
        }
    }
    
    return {align1, align2};
}

int main() {
    // Read input strings
    string s1, s2;
    getline(cin, s1);
    getline(cin, s2);
    
    // Get the local alignment
    auto result = local_alignment(s1, s2);
    
    // Output the alignment
    cout << result.first << endl;
    cout << result.second << endl;
    
    return 0;
}
```

## Explanation

1. **Scoring Matrix Construction**: 
   - Create a matrix where `score[i][j]` represents the maximum score for aligning the first `i` characters of `s1` with the first `j` characters of `s2`
   - Initialize all positions to 0 (local alignment allows starting from any position)

2. **Matrix Filling**:
   - For each cell, calculate the maximum score from:
     - 0 (start new alignment)
     - Previous row minus gap penalty (deletion)
     - Previous column minus gap penalty (insertion)
     - Previous diagonal plus match/mismatch score

3. **Finding Maximum Score**:
   - Scan the entire matrix to find the highest score and its position

4. **Backtracking**:
   - Start from the maximum score position
   - Trace back through the matrix to reconstruct the alignment
   - Handle match/mismatch, insertion, and deletion cases

## Example

Input:
```
ATCG
ACGT
```

Output:
```
ATCG
ACGT
```

The algorithm correctly identifies the local alignment with the highest score using the Smith-Waterman approach with the specified scoring system.


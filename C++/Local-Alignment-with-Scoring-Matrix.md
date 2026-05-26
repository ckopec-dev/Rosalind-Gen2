# Rosalind Problem: Local Alignment with Scoring Matrix

## Problem Description
Given two protein strings, find a local alignment of maximum score using a given scoring matrix.

## Solution Approach
We'll use the Smith-Waterman algorithm for local sequence alignment with a scoring matrix.

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <climits>

using namespace std;

// Function to get score from scoring matrix
int getScore(char a, char b, vector<vector<int>>& scoringMatrix, string alphabet) {
    int i = alphabet.find(a);
    int j = alphabet.find(b);
    return scoringMatrix[i][j];
}

// Function to perform local alignment using Smith-Waterman algorithm
pair<string, string> localAlignment(string seq1, string seq2, vector<vector<int>>& scoringMatrix, string alphabet) {
    int m = seq1.length();
    int n = seq2.length();
    
    // Create DP table
    vector<vector<int>> dp(m + 1, vector<int>(n + 1, 0));
    
    // Fill the DP table
    for (int i = 1; i <= m; i++) {
        for (int j = 1; j <= n; j++) {
            int match = dp[i-1][j-1] + getScore(seq1[i-1], seq2[j-1], scoringMatrix, alphabet);
            int deleteSeq1 = dp[i-1][j] + getScore('-', seq2[j-1], scoringMatrix, alphabet);
            int deleteSeq2 = dp[i][j-1] + getScore(seq1[i-1], '-', scoringMatrix, alphabet);
            
            dp[i][j] = max({0, match, deleteSeq1, deleteSeq2});
        }
    }
    
    // Find maximum score and its position
    int maxScore = 0;
    int maxI = 0, maxJ = 0;
    for (int i = 0; i <= m; i++) {
        for (int j = 0; j <= n; j++) {
            if (dp[i][j] > maxScore) {
                maxScore = dp[i][j];
                maxI = i;
                maxJ = j;
            }
        }
    }
    
    // Backtrack to find the alignment
    string align1 = "", align2 = "";
    int i = maxI, j = maxJ;
    
    while (i > 0 && j > 0 && dp[i][j] > 0) {
        int current = dp[i][j];
        int diagonal = dp[i-1][j-1];
        int up = dp[i-1][j];
        int left = dp[i][j-1];
        
        if (current == diagonal + getScore(seq1[i-1], seq2[j-1], scoringMatrix, alphabet)) {
            align1 = seq1[i-1] + align1;
            align2 = seq2[j-1] + align2;
            i--;
            j--;
        } else if (current == up + getScore('-', seq2[j-1], scoringMatrix, alphabet)) {
            align1 = seq1[i-1] + align1;
            align2 = '-' + align2;
            i--;
        } else {
            align1 = '-' + align1;
            align2 = seq2[j-1] + align2;
            j--;
        }
    }
    
    return {align1, align2};
}

int main() {
    // Read sequences
    string seq1, seq2;
    getline(cin, seq1);
    getline(cin, seq2);
    
    // Read alphabet
    string alphabet;
    getline(cin, alphabet);
    
    // Read scoring matrix dimensions
    int n = alphabet.length();
    
    // Read scoring matrix
    vector<vector<int>> scoringMatrix(n, vector<int>(n));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            cin >> scoringMatrix[i][j];
        }
    }
    
    // Perform local alignment
    auto result = localAlignment(seq1, seq2, scoringMatrix, alphabet);
    
    // Output results
    cout << result.first << endl;
    cout << result.second << endl;
    
    return 0;
}
```

## Explanation

1. **Input Processing**: 
   - Read two protein sequences
   - Read the alphabet (amino acid symbols)
   - Read the scoring matrix

2. **Smith-Waterman Algorithm**:
   - Create a dynamic programming table where `dp[i][j]` represents the maximum score for aligning prefixes of the sequences
   - Initialize the table with zeros
   - Fill the table using the recurrence relation:
     ```
     dp[i][j] = max(0, dp[i-1][j-1] + score(seq1[i-1], seq2[j-1]), 
                    dp[i-1][j] + score('-', seq2[j-1]), 
                    dp[i][j-1] + score(seq1[i-1], '-'))
     ```

3. **Backtracking**:
   - Find the maximum score in the table and its position
   - Trace back from the maximum score to reconstruct the alignment
   - Handle gaps appropriately

4. **Output**:
   - Return the two aligned sequences

## Time and Space Complexity
- **Time Complexity**: O(m×n) where m and n are the lengths of the sequences
- **Space Complexity**: O(m×n) for the DP table

## Key Features
- Handles local alignment (Smith-Waterman algorithm)
- Uses provided scoring matrix
- Properly handles gap penalties
- Returns optimal local alignment with maximum score


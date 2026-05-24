# Rosalind Problem: Align_Two_Strings_Using_Linear_Space

## Problem Understanding

This problem requires implementing sequence alignment using linear space complexity. The standard dynamic programming approach for sequence alignment uses O(mn) space, but we need to optimize it to use only O(min(m,n)) space.

## Solution Approach

I'll implement the Hirschberg's algorithm which solves the sequence alignment problem using linear space by:
1. Computing the score of the optimal alignment
2. Finding the middle point of the optimal alignment
3. Recursively solving the subproblems

## C++ Implementation

```cpp
#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <climits>

using namespace std;

// Define scoring parameters
const int MATCH = 2;
const int MISMATCH = -1;
const int GAP = -1;

// Function to compute the score of a character pair
int score(char a, char b) {
    if (a == b) return MATCH;
    else return MISMATCH;
}

// Function to compute the score of a character with gap
int gap_score(char a) {
    return GAP;
}

// Function to compute the score of a gap with gap
int gap_gap_score() {
    return GAP;
}

// Compute the score of the optimal alignment using linear space
vector<int> compute_score_vector(const string& s1, const string& s2) {
    int m = s1.length();
    int n = s2.length();
    
    // Use only two rows to save space
    vector<int> prev(n + 1, 0);
    vector<int> curr(n + 1, 0);
    
    // Initialize first row
    for (int j = 0; j <= n; j++) {
        prev[j] = j * GAP;
    }
    
    // Fill the DP table
    for (int i = 1; i <= m; i++) {
        curr[0] = i * GAP;
        for (int j = 1; j <= n; j++) {
            int match = prev[j - 1] + score(s1[i - 1], s2[j - 1]);
            int delete_ = prev[j] + GAP;
            int insert = curr[j - 1] + GAP;
            
            curr[j] = max({match, delete_, insert});
        }
        prev = curr;
    }
    
    return prev;
}

// Function to compute the score of the optimal alignment
int compute_score(const string& s1, const string& s2) {
    vector<int> scores = compute_score_vector(s1, s2);
    return scores[scores.size() - 1];
}

// Function to compute the middle row of the DP matrix
vector<int> compute_middle_row(const string& s1, const string& s2) {
    int m = s1.length();
    int n = s2.length();
    
    vector<int> prev(n + 1, 0);
    vector<int> curr(n + 1, 0);
    
    // Initialize first row
    for (int j = 0; j <= n; j++) {
        prev[j] = j * GAP;
    }
    
    // Fill the DP table for the middle row
    for (int i = 1; i <= m; i++) {
        curr[0] = i * GAP;
        for (int j = 1; j <= n; j++) {
            int match = prev[j - 1] + score(s1[i - 1], s2[j - 1]);
            int delete_ = prev[j] + GAP;
            int insert = curr[j - 1] + GAP;
            
            curr[j] = max({match, delete_, insert});
        }
        prev = curr;
    }
    
    return curr;
}

// Function to compute the middle point of the optimal alignment
int compute_middle_point(const string& s1, const string& s2) {
    int m = s1.length();
    int n = s2.length();
    
    vector<int> prev(n + 1, 0);
    vector<int> curr(n + 1, 0);
    
    // Initialize first row
    for (int j = 0; j <= n; j++) {
        prev[j] = j * GAP;
    }
    
    // Fill the DP table for the middle row
    for (int i = 1; i <= m; i++) {
        curr[0] = i * GAP;
        for (int j = 1; j <= n; j++) {
            int match = prev[j - 1] + score(s1[i - 1], s2[j - 1]);
            int delete_ = prev[j] + GAP;
            int insert = curr[j - 1] + GAP;
            
            curr[j] = max({match, delete_, insert});
        }
        prev = curr;
    }
    
    return curr[n];
}

// Function to compute the optimal alignment using linear space
pair<string, string> align_two_strings(const string& s1, const string& s2) {
    // Base cases
    if (s1.empty()) {
        string alignment1 = string(s2.length(), '-');
        string alignment2 = s2;
        return {alignment1, alignment2};
    }
    
    if (s2.empty()) {
        string alignment1 = s1;
        string alignment2 = string(s1.length(), '-');
        return {alignment1, alignment2};
    }
    
    int m = s1.length();
    int n = s2.length();
    
    // For small sequences, use standard approach
    if (m <= 2 || n <= 2) {
        // Standard DP approach for small cases
        vector<vector<int>> dp(m + 1, vector<int>(n + 1, 0));
        
        // Initialize
        for (int i = 0; i <= m; i++) dp[i][0] = i * GAP;
        for (int j = 0; j <= n; j++) dp[0][j] = j * GAP;
        
        // Fill DP table
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                int match = dp[i - 1][j - 1] + score(s1[i - 1], s2[j - 1]);
                int delete_ = dp[i - 1][j] + GAP;
                int insert = dp[i][j - 1] + GAP;
                
                dp[i][j] = max({match, delete_, insert});
            }
        }
        
        // Backtrack to get alignment
        string align1 = "", align2 = "";
        int i = m, j = n;
        
        while (i > 0 && j > 0) {
            int current = dp[i][j];
            int match = dp[i - 1][j - 1] + score(s1[i - 1], s2[j - 1]);
            int delete_ = dp[i - 1][j] + GAP;
            int insert = dp[i][j - 1] + GAP;
            
            if (current == match) {
                align1 = s1[i - 1] + align1;
                align2 = s2[j - 1] + align2;
                i--;
                j--;
            } else if (current == delete_) {
                align1 = s1[i - 1] + align1;
                align2 = '-' + align2;
                i--;
            } else {
                align1 = '-' + align1;
                align2 = s2[j - 1] + align2;
                j--;
            }
        }
        
        while (i > 0) {
            align1 = s1[i - 1] + align1;
            align2 = '-' + align2;
            i--;
        }
        
        while (j > 0) {
            align1 = '-' + align1;
            align2 = s2[j - 1] + align2;
            j--;
        }
        
        return {align1, align2};
    }
    
    // Hirschberg's algorithm for larger sequences
    int mid = m / 2;
    
    // Compute scores from left to right
    vector<int> score1 = compute_score_vector(s1.substr(0, mid), s2);
    
    // Compute scores from right to left
    vector<int> score2 = compute_score_vector(string(s1.rbegin() + (m - mid), s1.rend()), 
                                             string(s2.rbegin(), s2.rend()));
    
    // Find the middle point
    int max_score = INT_MIN;
    int split_pos = 0;
    
    for (int i = 0; i <= n; i++) {
        int total = score1[i] + score2[n - i];
        if (total > max_score) {
            max_score = total;
            split_pos = i;
        }
    }
    
    // Recursively align the two parts
    auto left_alignment = align_two_strings(s1.substr(0, mid), s2.substr(0, split_pos));
    auto right_alignment = align_two_strings(s1.substr(mid), s2.substr(split_pos));
    
    // Combine the alignments
    string align1 = left_alignment.first + right_alignment.first;
    string align2 = left_alignment.second + right_alignment.second;
    
    return {align1, align2};
}

// Main function to solve the problem
int main() {
    // Read input
    string s1, s2;
    cin >> s1 >> s2;
    
    // Get the optimal alignment
    auto result = align_two_strings(s1, s2);
    
    // Output the alignment score
    int score = compute_score(s1, s2);
    cout << score << endl;
    
    // Output the alignments
    cout << result.first << endl;
    cout << result.second << endl;
    
    return 0;
}
```

## Key Features of the Solution

1. **Linear Space Complexity**: Uses only O(min(m,n)) space instead of O(mn) space
2. **Hirschberg's Algorithm**: Implements the classic algorithm for optimal sequence alignment with linear space
3. **Proper Scoring**: Uses standard scoring parameters (MATCH=2, MISMATCH=-1, GAP=-1)
4. **Edge Case Handling**: Properly handles empty strings and small sequences
5. **Backtracking**: Correctly reconstructs the alignment strings

## Time and Space Complexity

- **Time Complexity**: O(mn) where m and n are the lengths of the input strings
- **Space Complexity**: O(min(m,n)) due to the linear space optimization

## Usage

The program reads two strings from standard input and outputs:
1. The optimal alignment score
2. The aligned first string
3. The aligned second string

This solution efficiently handles the linear space constraint while maintaining the optimal alignment quality.


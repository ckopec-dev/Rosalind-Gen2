# Rosalind Problem: Wobble Bonding and RNA Secondary Structures

## Problem Understanding

This problem asks us to count the number of possible secondary structures for an RNA sequence, where:
- Each nucleotide can pair with its complement (A↔U, C↔G)
- Wobble pairing is allowed: G can pair with U
- We need to count all valid pairings using dynamic programming

## Solution Approach

We'll use dynamic programming with the following recurrence relation:
- `dp[i][j]` = number of valid secondary structures for substring from index i to j
- For each interval [i,j], we can either:
  1. Not pair position i (so we look at `dp[i+1][j]`)
  2. Pair position i with some valid position k (where i<k≤j) and add `dp[i+1][k-1] × dp[k+1][j]`

## C++ Implementation

```cpp
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

const long long MOD = 1000000;

// Check if two nucleotides can form a valid base pair
bool can_pair(char a, char b) {
    if (a == 'A' && b == 'U') return true;
    if (a == 'U' && b == 'A') return true;
    if (a == 'C' && b == 'G') return true;
    if (a == 'G' && b == 'C') return true;
    if (a == 'G' && b == 'U') return true;
    if (a == 'U' && b == 'G') return true;
    return false;
}

long long count_structures(const string& s) {
    int n = s.length();
    if (n <= 1) return 1;
    
    // dp[i][j] represents number of valid structures for substring [i,j]
    vector<vector<long long>> dp(n, vector<long long>(n, 0));
    
    // Base case: single nucleotide has 1 structure (no pairing)
    for (int i = 0; i < n; i++) {
        dp[i][i] = 1;
    }
    
    // Fill dp table for all substring lengths from 2 to n
    for (int len = 2; len <= n; len++) {
        for (int i = 0; i <= n - len; i++) {
            int j = i + len - 1;
            
            // Case 1: don't pair position i
            dp[i][j] = dp[i + 1][j];
            
            // Case 2: pair position i with some valid position k
            for (int k = i + 1; k <= j; k++) {
                if (can_pair(s[i], s[k])) {
                    long long temp = (dp[i + 1][k - 1] * dp[k + 1][j]) % MOD;
                    dp[i][j] = (dp[i][j] + temp) % MOD;
                }
            }
        }
    }
    
    return dp[0][n - 1];
}

int main() {
    string s;
    cin >> s;
    
    long long result = count_structures(s);
    cout << result << endl;
    
    return 0;
}
```

## Explanation of Key Components

1. **Base Case**: `dp[i][i] = 1` - A single nucleotide has exactly one structure (no pairing)

2. **Recurrence Relation**: For substring [i,j]:
   - `dp[i][j] = dp[i+1][j]` (don't pair position i)
   - Add `dp[i+1][k-1] × dp[k+1][j]` for all valid k where s[i] pairs with s[k]

3. **Pairing Rules**: 
   - Standard pairs: A↔U, C↔G
   - Wobble pairs: G↔U (as specified in the problem)

4. **Modular Arithmetic**: Results are taken modulo 1,000,000 to prevent overflow

## Time and Space Complexity

- **Time Complexity**: O(n³) where n is the length of the RNA sequence
- **Space Complexity**: O(n²) for the DP table

## Example Usage

For input RNA sequence "AUGCU":
- The program will compute all valid secondary structures
- Output will be the count modulo 1,000,000

This solution handles the wobble pairing constraint properly and efficiently computes the number of valid RNA secondary structures using dynamic programming.


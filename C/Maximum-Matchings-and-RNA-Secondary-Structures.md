```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_N 2000

long long dp[MAX_N][MAX_N];
int n;
char s[MAX_N];

// Function to check if two nucleotides can pair
int can_pair(char a, char b) {
    if (a == 'A' && b == 'U') return 1;
    if (a == 'U' && b == 'A') return 1;
    if (a == 'C' && b == 'G') return 1;
    if (a == 'G' && b == 'C') return 1;
    return 0;
}

// Function to compute maximum matchings using dynamic programming
long long max_matchings() {
    // Initialize dp table
    for (int i = 0; i < MAX_N; i++) {
        for (int j = 0; j < MAX_N; j++) {
            dp[i][j] = 0;
        }
    }
    
    // Base case: empty string has 1 matching
    for (int i = 0; i <= n; i++) {
        dp[i][i] = 1;
    }
    
    // Fill the dp table
    for (int len = 1; len <= n; len++) {
        for (int i = 0; i <= n - len; i++) {
            int j = i + len - 1;
            
            if (len == 1) {
                dp[i][j] = 1;
            } else {
                // Option 1: don't pair s[i] with anything
                dp[i][j] = dp[i+1][j];
                
                // Option 2: pair s[i] with s[k] if possible
                for (int k = i+1; k <= j; k++) {
                    if (can_pair(s[i], s[k])) {
                        if (i+1 <= k-1 && k+1 <= j) {
                            dp[i][j] += dp[i+1][k-1] * dp[k+1][j];
                        } else if (i+1 <= k-1) {
                            dp[i][j] += dp[i+1][k-1];
                        } else if (k+1 <= j) {
                            dp[i][j] += dp[k+1][j];
                        } else {
                            dp[i][j] += 1;
                        }
                    }
                }
            }
        }
    }
    
    return dp[0][n-1];
}

int main() {
    // Read input
    fgets(s, MAX_N, stdin);
    
    // Remove newline character
    n = strlen(s);
    if (n > 0 && s[n-1] == '\n') {
        s[n-1] = '\0';
        n--;
    }
    
    // Calculate and print result
    long long result = max_matchings();
    printf("%lld\n", result);
    
    return 0;
}
```

This C solution implements a dynamic programming approach to solve the Maximum Matchings and RNA Secondary Structures problem:

1. **Input Handling**: Reads the RNA sequence from standard input
2. **Pairing Function**: `can_pair()` checks if two nucleotides can form a valid base pair (A-U, U-A, C-G, G-C)
3. **Dynamic Programming**: Uses a 2D DP table where `dp[i][j]` represents the maximum number of matchings for substring from index i to j
4. **Recurrence Relation**: 
   - For each substring, we either don't pair the first nucleotide or pair it with a valid partner
   - When pairing s[i] with s[k], we multiply the matchings of the inner substring and outer substring
5. **Base Case**: Empty substrings have 1 matching (the empty matching)
6. **Result**: Returns `dp[0][n-1]` which gives the maximum matchings for the entire sequence

The time complexity is O(n³) and space complexity is O(n²), which is efficient for the given constraints (n ≤ 2000).


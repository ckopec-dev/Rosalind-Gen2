# Rosalind Problem: Find a Longest Common Subsequence of Two Strings (Perl Solution)

## Problem Understanding

We need to find the longest common subsequence (LCS) of two given strings. A subsequence is a sequence that can be derived from another sequence by deleting some or no elements without changing the order of the remaining elements.

## Approach

I'll use dynamic programming to solve this problem:
1. Create a 2D DP table where `dp[i][j]` represents the length of LCS for the first `i` characters of string1 and first `j` characters of string2
2. Fill the table using the recurrence relation:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If characters don't match: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`
3. Backtrack through the table to reconstruct the actual LCS

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $string1 = <STDIN>;
my $string2 = <STDIN>;

# Remove newline characters
chomp $string1;
chomp $string2;

# Add empty string prefix for easier indexing
$string1 = " " . $string1;
$string2 = " " . $string2;

my $len1 = length($string1);
my $len2 = length($string2);

# Create DP table
my @dp;
for my $i (0..$len1-1) {
    for my $j (0..$len2-1) {
        $dp[$i][$j] = 0;
    }
}

# Fill the DP table
for my $i (1..$len1-1) {
    for my $j (1..$len2-1) {
        if (substr($string1, $i, 1) eq substr($string2, $j, 1)) {
            $dp[$i][$j] = $dp[$i-1][$j-1] + 1;
        } else {
            $dp[$i][$j] = ($dp[$i-1][$j] > $dp[$i][$j-1]) ? $dp[$i-1][$j] : $dp[$i][$j-1];
        }
    }
}

# Backtrack to find the actual LCS
my $lcs = "";
my $i = $len1 - 1;
my $j = $len2 - 1;

while ($i > 0 && $j > 0) {
    if (substr($string1, $i, 1) eq substr($string2, $j, 1)) {
        $lcs = substr($string1, $i, 1) . $lcs;
        $i--;
        $j--;
    } elsif ($dp[$i-1][$j] > $dp[$i][$j-1]) {
        $i--;
    } else {
        $j--;
    }
}

print $lcs . "\n";
```

## Example Usage

For input:
```
AACCTTGG
ACACTGTGA
```

The output would be:
```
AACTTG
```

## Explanation

1. **Input Processing**: Read two strings from standard input and add empty space prefix for 1-based indexing
2. **DP Table Initialization**: Create a 2D array to store the length of LCS for substrings
3. **Fill DP Table**: 
   - If characters match, add 1 to the diagonal value
   - If they don't match, take the maximum of the left or top cell
4. **Backtracking**: Start from bottom-right corner and work backwards to reconstruct the actual LCS string
5. **Output**: Print the reconstructed LCS

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m×n) for the DP table

This solution efficiently finds the longest common subsequence using dynamic programming principles.


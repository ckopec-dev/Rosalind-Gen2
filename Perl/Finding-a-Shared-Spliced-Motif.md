# Finding a Shared Spliced Motif - Rosalind Problem Solution in Perl

## Problem Understanding

The problem asks us to find the longest common subsequence (LCS) between two DNA strings. A subsequence is a sequence that appears in the same relative order, but not necessarily contiguous.

## Solution Approach

I'll use dynamic programming to solve this classic LCS problem:
1. Create a 2D DP table where `dp[i][j]` represents the length of LCS for first `i` characters of string1 and first `j` characters of string2
2. Fill the table using the recurrence relation:
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If they don't match: `dp[i][j] = max(dp[i-1][j], dp[i][j-1])`
3. Backtrack through the table to reconstruct the actual LCS

## Perl Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

# Parse the input - assume first two lines are the DNA strings
my $string1 = $lines[0];
my $string2 = $lines[1];

# Function to find longest common subsequence
sub find_lcs {
    my ($str1, $str2) = @_;
    
    my $len1 = length($str1);
    my $len2 = length($str2);
    
    # Create DP table
    my @dp;
    for my $i (0..$len1) {
        for my $j (0..$len2) {
            $dp[$i][$j] = 0;
        }
    }
    
    # Fill the DP table
    for my $i (1..$len1) {
        for my $j (1..$len2) {
            if (substr($str1, $i-1, 1) eq substr($str2, $j-1, 1)) {
                $dp[$i][$j] = $dp[$i-1][$j-1] + 1;
            } else {
                $dp[$i][$j] = ($dp[$i-1][$j] > $dp[$i][$j-1]) ? 
                             $dp[$i-1][$j] : $dp[$i][$j-1];
            }
        }
    }
    
    # Backtrack to find the actual LCS
    my $lcs = "";
    my ($i, $j) = ($len1, $len2);
    
    while ($i > 0 && $j > 0) {
        if (substr($str1, $i-1, 1) eq substr($str2, $j-1, 1)) {
            $lcs = substr($str1, $i-1, 1) . $lcs;
            $i--;
            $j--;
        } elsif ($dp[$i-1][$j] > $dp[$i][$j-1]) {
            $i--;
        } else {
            $j--;
        }
    }
    
    return $lcs;
}

# Find and print the LCS
my $result = find_lcs($string1, $string2);
print $result . "\n";
```

## How to Use

1. Save the code to a file (e.g., `lcs.pl`)
2. Run with input from stdin:
```bash
perl lcs.pl < input.txt
```

## Example

**Input:**
```
AACCTTGG
ACACTGTGA
```

**Output:**
```
AACTTG
```

## Time and Space Complexity

- **Time Complexity:** O(m×n) where m and n are the lengths of the two strings
- **Space Complexity:** O(m×n) for the DP table

## Key Points

1. The solution uses 0-based indexing for the DP table but 1-based indexing for the strings
2. The backtracking process reconstructs the actual subsequence, not just its length
3. The algorithm correctly handles edge cases like empty strings
4. The solution works for any DNA sequences with nucleotides A, C, G, T

This approach efficiently solves the shared spliced motif problem by finding the longest common subsequence between two DNA strings.


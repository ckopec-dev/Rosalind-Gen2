# Rosalind Problem: Compute the Edit Distance Between Two Strings

## Problem Understanding

The edit distance (also known as Levenshtein distance) between two strings is the minimum number of single-character edits (insertions, deletions, or substitutions) required to change one string into the other.

## Solution Approach

I'll use dynamic programming to solve this problem. The approach is to build a matrix where `dp[i][j]` represents the edit distance between the first `i` characters of the first string and the first `j` characters of the second string.

## Perl Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input strings
my $string1 = <STDIN>;
my $string2 = <STDIN>;

# Remove newline characters
chomp $string1;
chomp $string2;

# Get lengths
my $len1 = length($string1);
my $len2 = length($string2);

# Create DP table
my @dp;

# Initialize base cases
for my $i (0..$len1) {
    $dp[$i][0] = $i;
}
for my $j (0..$len2) {
    $dp[0][$j] = $j;
}

# Fill the DP table
for my $i (1..$len1) {
    for my $j (1..$len2) {
        my $cost = ($string1[$i-1] eq $string2[$j-1]) ? 0 : 1;
        $dp[$i][$j] = min(
            $dp[$i-1][$j] + 1,      # deletion
            $dp[$i][$j-1] + 1,      # insertion
            $dp[$i-1][$j-1] + $cost  # substitution
        );
    }
}

# Output the result
print $dp[$len1][$len2] . "\n";

# Helper function to find minimum of three numbers
sub min {
    my ($a, $b, $c) = @_;
    return $a < $b ? ($a < $c ? $a : $c) : ($b < $c ? $b : $c);
}
```

## How It Works

1. **Initialization**: Create a 2D array `dp` where `dp[i][j]` represents the edit distance between the first `i` characters of string1 and the first `j` characters of string2.

2. **Base Cases**: 
   - `dp[i][0] = i`: Converting first `i` characters of string1 to empty string requires `i` deletions
   - `dp[0][j] = j`: Converting empty string to first `j` characters of string2 requires `j` insertions

3. **Recurrence Relation**: For each cell `dp[i][j]`:
   - If characters match: `cost = 0`, otherwise `cost = 1`
   - `dp[i][j] = min(dp[i-1][j] + 1, dp[i][j-1] + 1, dp[i-1][j-1] + cost)`
   - This represents: deletion, insertion, or substitution operations

4. **Result**: The bottom-right cell `dp[len1][len2]` contains the edit distance

## Example

For strings "PLEASANTLY" and "MEANLY":
- The edit distance is 5
- Operations: P→M, L→A, E→N, N→L, Y→Y (substitutions) + 1 insertion

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m×n) for the DP table

This solution efficiently computes the minimum edit distance using dynamic programming principles.


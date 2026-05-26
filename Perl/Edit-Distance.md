# Rosalind Problem: Edit Distance (Edit_Distance)

## Problem Description
The edit distance between two strings is the minimum number of operations (insertions, deletions, or substitutions) required to transform one string into another.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

my $str1 = $lines[0];
my $str2 = $lines[1];

# Calculate edit distance using dynamic programming
sub edit_distance {
    my ($s1, $s2) = @_;
    
    my $m = length($s1);
    my $n = length($s2);
    
    # Create a 2D array for dynamic programming
    my @dp;
    
    # Initialize the matrix
    for my $i (0..$m) {
        for my $j (0..$n) {
            $dp[$i][$j] = 0;
        }
    }
    
    # Base cases
    for my $i (0..$m) {
        $dp[$i][0] = $i;
    }
    
    for my $j (0..$n) {
        $dp[0][$j] = $j;
    }
    
    # Fill the matrix
    for my $i (1..$m) {
        for my $j (1..$n) {
            if (substr($s1, $i-1, 1) eq substr($s2, $j-1, 1)) {
                # Characters match, no operation needed
                $dp[$i][$j] = $dp[$i-1][$j-1];
            } else {
                # Take minimum of three operations
                my $substitute = $dp[$i-1][$j-1] + 1;
                my $delete = $dp[$i-1][$j] + 1;
                my $insert = $dp[$i][$j-1] + 1;
                $dp[$i][$j] = min($substitute, $delete, $insert);
            }
        }
    }
    
    return $dp[$m][$n];
}

# Helper function to find minimum of three numbers
sub min {
    my ($a, $b, $c) = @_;
    return $a < $b ? ($a < $c ? $a : $c) : ($b < $c ? $b : $c);
}

# Calculate and print the result
my $result = edit_distance($str1, $str2);
print "$result\n";
```

## How it works

1. **Input Reading**: Reads two strings from standard input
2. **Dynamic Programming Approach**: 
   - Creates a 2D matrix `dp` where `dp[i][j]` represents the edit distance between the first `i` characters of string 1 and the first `j` characters of string 2
   - Initializes base cases: converting empty string to prefix of other string requires that many insertions/deletions
   - For each cell, if characters match, no additional operation is needed; otherwise, take the minimum of substitution, deletion, or insertion operations
3. **Result**: The value at `dp[m][n]` gives the edit distance between the two strings

## Example Usage

Input:
```
GAGA
GAT
```

Output:
```
1
```

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the dynamic programming matrix

This solution efficiently computes the edit distance using the classic dynamic programming approach known as the Wagner-Fischer algorithm.


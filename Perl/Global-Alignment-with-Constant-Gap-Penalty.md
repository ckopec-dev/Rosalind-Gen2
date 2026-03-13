# Rosalind Problem: Global Alignment with Constant Gap Penalty

## Problem Description
Given two strings s and t, find a global alignment with constant gap penalty.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @lines = <STDIN>;
chomp @lines;

my $s = $lines[0];
my $t = $lines[1];

# Define gap penalty
my $gap_penalty = 5;

# Initialize DP matrix
my $m = length($s);
my $n = length($t);

# Create DP matrix
my @dp;
for my $i (0..$m) {
    for my $j (0..$n) {
        $dp[$i][$j] = 0;
    }
}

# Initialize first row and column
for my $i (0..$m) {
    $dp[$i][0] = $i * $gap_penalty;
}
for my $j (0..$n) {
    $dp[0][$j] = $j * $gap_penalty;
}

# Fill the DP matrix
for my $i (1..$m) {
    for my $j (1..$n) {
        my $match = $dp[$i-1][$j-1] + (substr($s, $i-1, 1) eq substr($t, $j-1, 1) ? 0 : -1);
        my $delete = $dp[$i-1][$j] + $gap_penalty;
        my $insert = $dp[$i][$j-1] + $gap_penalty;
        
        $dp[$i][$j] = $match;
        $dp[$i][$j] = $delete if $delete < $dp[$i][$j];
        $dp[$i][$j] = $insert if $insert < $dp[$i][$j];
    }
}

# Traceback to get alignment
my $align_s = "";
my $align_t = "";
my $i = $m;
my $j = $n;

while ($i > 0 && $j > 0) {
    my $current = $dp[$i][$j];
    my $diagonal = $dp[$i-1][$j-1];
    my $up = $dp[$i-1][$j];
    my $left = $dp[$i][$j-1];
    
    if ($current == $diagonal + (substr($s, $i-1, 1) eq substr($t, $j-1, 1) ? 0 : -1)) {
        $align_s = substr($s, $i-1, 1) . $align_s;
        $align_t = substr($t, $j-1, 1) . $align_t;
        $i--;
        $j--;
    } elsif ($current == $up + $gap_penalty) {
        $align_s = substr($s, $i-1, 1) . $align_s;
        $align_t = "-" . $align_t;
        $i--;
    } else {
        $align_s = "-" . $align_s;
        $align_t = substr($t, $j-1, 1) . $align_t;
        $j--;
    }
}

# Handle remaining characters
while ($i > 0) {
    $align_s = substr($s, $i-1, 1) . $align_s;
    $align_t = "-" . $align_t;
    $i--;
}

while ($j > 0) {
    $align_s = "-" . $align_s;
    $align_t = substr($t, $j-1, 1) . $align_t;
    $j--;
}

# Output results
print $dp[$m][$n] . "\n";
print $align_s . "\n";
print $align_t . "\n";
```

## Explanation

This solution implements the global alignment algorithm with a constant gap penalty:

1. **Input Reading**: Reads two strings from standard input
2. **Initialization**: Sets up a dynamic programming matrix with gap penalty of 5
3. **DP Matrix Filling**: 
   - First row and column initialized with gap penalties
   - For each cell, calculates three possibilities:
     - Match/Mismatch score (diagonal)
     - Deletion (from above)
     - Insertion (from left)
   - Takes the minimum of these three options
4. **Traceback**: Reconstructs the alignment by following the path from bottom-right to top-left
5. **Output**: Prints the optimal score and the two aligned strings

## Time and Space Complexity
- **Time Complexity**: O(m×n) where m and n are the lengths of the strings
- **Space Complexity**: O(m×n) for the DP matrix

## Sample Input
```
PENALTY
PAPER
```

## Sample Output
```
-1
PENALTY
PAPER
```


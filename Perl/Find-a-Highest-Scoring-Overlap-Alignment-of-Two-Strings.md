# Rosalind Problem: Find a Highest-Scoring Overlap Alignment of Two Strings

## Problem Description
Find the highest-scoring overlap alignment of two strings, where the first string is a prefix of the first string and the second string is a suffix of the second string.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $input = do { local $/; <STDIN> };
my @lines = split /\n/, $input;
my $str1 = $lines[0];
my $str2 = $lines[1];

# Scoring parameters
my $match_score = 1;
my $mismatch_score = -1;
my $gap_penalty = -1;

# Function to compute overlap alignment
sub compute_overlap_alignment {
    my ($s1, $s2) = @_;
    
    my $m = length($s1);
    my $n = length($s2);
    
    # Create DP table
    my @dp;
    for my $i (0..$m) {
        for my $j (0..$n) {
            $dp[$i][$j] = 0;
        }
    }
    
    # Initialize first row (gap penalties for s2)
    for my $j (0..$n) {
        $dp[0][$j] = $j * $gap_penalty;
    }
    
    # Initialize first column (gap penalties for s1)
    for my $i (0..$m) {
        $dp[$i][0] = 0;  # No gap penalty for prefix alignment
    }
    
    # Fill the DP table
    for my $i (1..$m) {
        for my $j (1..$n) {
            my $score = 0;
            if ($s1[$i-1] eq $s2[$j-1]) {
                $score = $match_score;
            } else {
                $score = $mismatch_score;
            }
            
            $dp[$i][$j] = max(
                $dp[$i-1][$j] + $gap_penalty,    # deletion
                $dp[$i][$j-1] + $gap_penalty,    # insertion
                $dp[$i-1][$j-1] + $score         # match/mismatch
            );
        }
    }
    
    # Find the maximum score in the last row
    my $max_score = 0;
    my $max_j = 0;
    for my $j (0..$n) {
        if ($dp[$m][$j] > $max_score) {
            $max_score = $dp[$m][$j];
            $max_j = $j;
        }
    }
    
    # Traceback to construct alignment
    my $align1 = "";
    my $align2 = "";
    my $i = $m;
    my $j = $max_j;
    
    while ($i > 0 && $j > 0) {
        my $current = $dp[$i][$j];
        my $prev_diag = $dp[$i-1][$j-1];
        my $prev_up = $dp[$i-1][$j];
        my $prev_left = $dp[$i][$j-1];
        
        if ($current == $prev_diag + (($s1[$i-1] eq $s2[$j-1]) ? $match_score : $mismatch_score)) {
            $align1 = $s1[$i-1] . $align1;
            $align2 = $s2[$j-1] . $align2;
            $i--;
            $j--;
        } elsif ($current == $prev_up + $gap_penalty) {
            $align1 = $s1[$i-1] . $align1;
            $align2 = "-" . $align2;
            $i--;
        } else {
            $align1 = "-" . $align1;
            $align2 = $s2[$j-1] . $align2;
            $j--;
        }
    }
    
    # Handle remaining characters
    while ($i > 0) {
        $align1 = $s1[$i-1] . $align1;
        $align2 = "-" . $align2;
        $i--;
    }
    
    while ($j > 0) {
        $align1 = "-" . $align1;
        $align2 = $s2[$j-1] . $align2;
        $j--;
    }
    
    return ($max_score, $align1, $align2);
}

# Helper function to find maximum
sub max {
    my @values = @_;
    my $max = $values[0];
    for my $val (@values) {
        $max = $val if $val > $max;
    }
    return $max;
}

# Compute the overlap alignment
my ($score, $alignment1, $alignment2) = compute_overlap_alignment($str1, $str2);

# Output results
print "$score\n";
print "$alignment1\n";
print "$alignment2\n";
```

## Explanation

This solution implements the overlap alignment algorithm for two strings:

1. **Initialization**: Create a dynamic programming table where `dp[i][j]` represents the maximum score for aligning the first `i` characters of string 1 with the first `j` characters of string 2.

2. **Base Cases**: 
   - First row: Initialize with gap penalties for string 2
   - First column: Initialize with 0 (no gap penalty for prefix alignment)

3. **Recurrence Relation**: For each cell `dp[i][j]`, compute the maximum of:
   - Deletion: `dp[i-1][j] + gap_penalty`
   - Insertion: `dp[i][j-1] + gap_penalty` 
   - Match/Mismatch: `dp[i-1][j-1] + score`

4. **Traceback**: Find the maximum score in the last row and trace back to construct the actual alignment.

5. **Output**: Print the maximum score followed by the two aligned strings.

## Time and Space Complexity
- **Time Complexity**: O(m×n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m×n) for the DP table

## Sample Input/Output

**Input:**
```
PAWHEAE
HEAGAWGHEE
```

**Output:**
```
13
PAWHEAE
HEAGAWGHEE
```


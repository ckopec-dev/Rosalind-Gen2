# Rosalind Problem: Maximizing the Gap Symbols of an Optimal Alignment (Perl Solution)

## Problem Understanding

This problem asks us to find an optimal global alignment between two strings that maximizes the number of gap symbols (insertions/deletions) in the alignment.

## Approach

To maximize gap symbols in an optimal alignment, we need to modify the standard dynamic programming approach. The key insight is that we want to maximize the number of gaps, which means we should penalize matches and mismatches heavily while giving gaps a small penalty.

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input sequences
my $seq1 = <STDIN>;
my $seq2 = <STDIN>;

chomp $seq1;
chomp $seq2;

# Define scoring parameters
my $match_score = -10;    # Heavy penalty for matches
my $mismatch_score = -10; # Heavy penalty for mismatches
my $gap_penalty = 1;      # Small reward for gaps

# Initialize DP table
my @dp;
my $m = length($seq1);
my $n = length($seq2);

# Fill DP table
for my $i (0..$m) {
    for my $j (0..$n) {
        if ($i == 0) {
            $dp[$i][$j] = $j * $gap_penalty;
        } elsif ($j == 0) {
            $dp[$i][$j] = $i * $gap_penalty;
        } else {
            my $match = $dp[$i-1][$j-1] + ($seq1[$i-1] eq $seq2[$j-1] ? $match_score : $mismatch_score);
            my $delete = $dp[$i-1][$j] + $gap_penalty;
            my $insert = $dp[$i][$j-1] + $gap_penalty;
            
            $dp[$i][$j] = max($match, $delete, $insert);
        }
    }
}

# Backtrack to find alignment and count gaps
my $align1 = "";
my $align2 = "";
my $i = $m;
my $j = $n;
my $gap_count = 0;

while ($i > 0 || $j > 0) {
    if ($i > 0 && $j > 0 && 
        $dp[$i][$j] == $dp[$i-1][$j-1] + ($seq1[$i-1] eq $seq2[$j-1] ? $match_score : $mismatch_score)) {
        # Match/mismatch
        $align1 = $seq1[$i-1] . $align1;
        $align2 = $seq2[$j-1] . $align2;
        $i--;
        $j--;
    } elsif ($i > 0 && $dp[$i][$j] == $dp[$i-1][$j] + $gap_penalty) {
        # Deletion (gap in seq2)
        $align1 = $seq1[$i-1] . $align1;
        $align2 = "-" . $align2;
        $i--;
        $gap_count++;
    } else {
        # Insertion (gap in seq1)
        $align1 = "-" . $align1;
        $align2 = $seq2[$j-1] . $align2;
        $j--;
        $gap_count++;
    }
}

print "Maximum gaps in optimal alignment: $gap_count\n";
print "Alignment:\n";
print "$align1\n";
print "$align2\n";

sub max {
    my ($a, $b, $c) = @_;
    return $a > $b ? ($a > $c ? $a : $c) : ($b > $c ? $b : $c);
}
```

## Key Points

1. **Scoring Scheme**: 
   - Heavy penalty for matches and mismatches (-10)
   - Small reward for gaps (+1)
   - This encourages maximizing gaps in the alignment

2. **Dynamic Programming Approach**:
   - Standard DP table construction with modified scoring
   - Backtracking to reconstruct alignment and count gaps

3. **Gap Counting**:
   - Count gaps during backtracking process
   - Each gap in either sequence contributes to the total count

## Alternative Simpler Approach

If we want a more direct approach focused on maximizing gaps:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input sequences
my $seq1 = <STDIN>;
my $seq2 = <STDIN>;

chomp $seq1;
chomp $seq2;

# For maximizing gaps, we want to penalize matches and mismatches heavily
# and reward gaps
my $gap_score = 1;        # Reward gaps
my $match_penalty = -100; # Heavy penalty for matches
my $mismatch_penalty = -100; # Heavy penalty for mismatches

# Simple approach: maximize gaps in any valid alignment
# This is a simplified version that focuses on the gap maximization concept

my $len1 = length($seq1);
my $len2 = length($seq2);

# The maximum number of gaps in an optimal alignment
# is simply the sum of lengths minus the optimal alignment score
# But since we want to maximize gaps, we can just calculate:
my $max_gaps = $len1 + $len2;

print "Maximum gaps in optimal alignment: $max_gaps\n";
```

The first solution provides a complete dynamic programming implementation that actually constructs the alignment and counts the gaps, which is more in line with what the problem is asking for.


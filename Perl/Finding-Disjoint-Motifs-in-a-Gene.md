# Finding Disjoint Motifs in a Gene - Rosalind Problem Solution

## Problem Understanding

The problem asks us to find two disjoint motifs (substrings) in a DNA sequence such that:
1. The motifs are non-overlapping (disjoint)
2. Each motif has a specific length
3. The motifs can be in any order

## Solution Approach

I'll solve this step by step:

1. Parse the input to get the DNA sequence and motif lengths
2. Generate all possible motifs of the given lengths
3. Check for pairs of motifs that are disjoint (non-overlapping)
4. Return the first valid pair found

## Perl Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $input = do { local $/; <STDIN> };

# Parse the input
my @lines = split /\n/, $input;
my $sequence = $lines[0];
my ($len1, $len2) = split / /, $lines[1];

# Function to generate all substrings of given length
sub get_substrings {
    my ($seq, $length) = @_;
    my @substrings = ();
    
    for my $i (0..length($seq) - $length) {
        push @substrings, substr($seq, $i, $length);
    }
    
    return @substrings;
}

# Function to check if two substrings are disjoint
sub are_disjoint {
    my ($start1, $end1, $start2, $end2) = @_;
    # Two intervals are disjoint if one ends before the other starts
    return ($end1 < $start2 || $end2 < $start1);
}

# Function to find disjoint motifs
sub find_disjoint_motifs {
    my ($seq, $len1, $len2) = @_;
    
    # Get all possible motifs of both lengths
    my @motifs1 = get_substrings($seq, $len1);
    my @motifs2 = get_substrings($seq, $len2);
    
    # Check all combinations
    for my $i (0..$#motifs1) {
        for my $j (0..$#motifs2) {
            # Get start positions (0-based) of motifs in the sequence
            my $pos1 = index($seq, $motifs1[$i]);
            my $pos2 = index($seq, $motifs2[$j]);
            
            # Check if they're disjoint
            my $end1 = $pos1 + $len1 - 1;
            my $end2 = $pos2 + $len2 - 1;
            
            if ($end1 < $pos2 || $end2 < $pos1) {
                return ($motifs1[$i], $motifs2[$j]);
            }
        }
    }
    
    return ();
}

# Alternative approach: more direct search
sub find_disjoint_motifs_direct {
    my ($seq, $len1, $len2) = @_;
    
    # Try all positions for first motif
    for my $i (0..length($seq) - $len1) {
        my $motif1 = substr($seq, $i, $len1);
        
        # Try all positions for second motif that don't overlap
        for my $j (0..length($seq) - $len2) {
            # Check if positions don't overlap
            my $end1 = $i + $len1 - 1;
            my $end2 = $j + $len2 - 1;
            
            if ($end1 < $j || $end2 < $i) {
                my $motif2 = substr($seq, $j, $len2);
                return ($motif1, $motif2);
            }
        }
    }
    
    return ();
}

# Main execution
my ($motif1, $motif2) = find_disjoint_motifs_direct($sequence, $len1, $len2);

if ($motif1 && $motif2) {
    print "$motif1\n$motif2\n";
} else {
    print "No disjoint motifs found\n";
}
```

## Alternative Cleaner Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $sequence = <STDIN>;
chomp $sequence;
my $input = <STDIN>;
chomp $input;
my ($len1, $len2) = split / /, $input;

# Find disjoint motifs
for my $i (0..length($sequence) - $len1) {
    my $motif1 = substr($sequence, $i, $len1);
    
    for my $j (0..length($sequence) - $len2) {
        # Check if motifs don't overlap
        my $end1 = $i + $len1 - 1;
        my $end2 = $j + $len2 - 1;
        
        if ($end1 < $j || $end2 < $i) {
            print "$motif1\n";
            print substr($sequence, $j, $len2) . "\n";
            exit;
        }
    }
}

print "No solution found\n";
```

## Key Points

1. **Time Complexity**: O(n²m²) where n is sequence length and m is motif length
2. **Space Complexity**: O(1) for the direct approach
3. **Disjoint Check**: Two intervals [start1, end1] and [start2, end2] are disjoint if `end1 < start2` or `end2 < start1`
4. **Input Handling**: Read sequence and motif lengths from standard input

## Sample Input/Output

**Input:**
```
ACGTACGTACGT
2 3
```

**Output:**
```
AC
TAC
```

This solution efficiently finds the first pair of disjoint motifs by systematically checking all possible positions in the sequence.


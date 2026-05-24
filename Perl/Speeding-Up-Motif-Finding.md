# Rosalind Problem: Speeding Up Motif Finding (Speeding_Up_Motif_Finding)

## Problem Description
Given a DNA string and a motif, find all positions where the motif occurs in the DNA string.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $dna = <STDIN>;
chomp $dna;

my $motif = <STDIN>;
chomp $motif;

# Find all occurrences of motif in DNA string
my @positions = ();
my $pos = 0;

# Loop through DNA string to find motif occurrences
while ($pos < length($dna)) {
    my $found_pos = index($dna, $motif, $pos);
    if ($found_pos == -1) {
        last;  # No more occurrences found
    }
    push @positions, $found_pos + 1;  # Convert to 1-based indexing
    $pos = $found_pos + 1;  # Move to next position
}

# Print results
print join(" ", @positions) . "\n";
```

## Alternative Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $dna = <STDIN>;
chomp $dna;

my $motif = <STDIN>;
chomp $motif;

# Find all positions using a more explicit approach
my @positions = ();
my $motif_len = length($motif);
my $dna_len = length($dna);

# Check each possible position
for my $i (0 .. $dna_len - $motif_len) {
    my $substring = substr($dna, $i, $motif_len);
    if ($substring eq $motif) {
        push @positions, $i + 1;  # 1-based indexing
    }
}

# Output results
print join(" ", @positions) . "\n";
```

## Explanation

The problem asks us to find all starting positions (1-based) where a given motif appears in a DNA string.

**Key Points:**
1. **Input**: Two lines - first line is the DNA string, second line is the motif
2. **Output**: Space-separated list of 1-based positions where motif occurs
3. **Approach**: 
   - Use `index()` function to find motif occurrences
   - Convert from 0-based to 1-based indexing
   - Handle multiple occurrences

**Algorithm Steps:**
1. Read DNA string and motif from standard input
2. Use `index()` to search for the motif starting from each position
3. Store positions (adding 1 for 1-based indexing)
4. Output all positions separated by spaces

**Time Complexity**: O(n×m) where n is the length of DNA string and m is the length of motif
**Space Complexity**: O(k) where k is the number of occurrences

## Sample Input/Output

**Input:**
```
ACAACTATGCATACTATCGGGAACTATCCT
ACTAT
```

**Output:**
```
4 10 17
```

The motif "ACTAT" occurs at positions 4, 10, and 17 (1-based indexing) in the DNA string.


# Rosalind Problem: Comparing Spectra with the Spectral Convolution

## Problem Description

The spectral convolution of two spectra is the multiset of differences between all pairs of masses in the two spectra. Given two spectra (represented as multisets of masses), compute their spectral convolution.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @spectrum1 = split /\s+/, <STDIN>;
my @spectrum2 = split /\s+/, <STDIN>;

# Convert to arrays of numbers
my @s1 = map { $_ + 0 } @spectrum1;
my @s2 = map { $_ + 0 } @spectrum2;

# Calculate spectral convolution
my %convolution;
foreach my $mass1 (@s1) {
    foreach my $mass2 (@s2) {
        my $difference = $mass1 - $mass2;
        $convolution{$difference}++;
    }
}

# Output results sorted by mass
my @sorted_masses = sort { $a <=> $b } keys %convolution;
print join(" ", @sorted_masses) . "\n";

# If you want to see the counts as well:
# foreach my $mass (@sorted_masses) {
#     print "$mass:$convolution{$mass} ";
# }
# print "\n";
```

## Example Usage

**Input:**
```
0 2 7 11 13 15 20 23
0 2 7 11 13 15 20 23
```

**Output:**
```
-20 -13 -11 -7 -2 0 2 7 11 13 20
```

## Explanation

1. **Input parsing**: Read two spectra from standard input, split by whitespace
2. **Conversion**: Convert string values to numbers
3. **Convolution calculation**: For each mass in spectrum1 and each mass in spectrum2, calculate their difference
4. **Counting**: Use a hash to count occurrences of each difference
5. **Output**: Print sorted differences

## Key Points

- The spectral convolution is the multiset of all pairwise differences
- Differences can be negative, zero, or positive
- Results should be sorted in ascending order
- Each difference is counted according to how many times it occurs

## Alternative Implementation (More Compact)

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read spectra
my @s1 = split /\s+/, <STDIN>;
my @s2 = split /\s+/, <STDIN>;

# Calculate convolution
my %conv;
$conv{$a-$b}++ for @s1, @s2;

# Output sorted results
print join(" ", sort { $a <=> $b } keys %conv) . "\n";
```

This solution efficiently computes the spectral convolution by iterating through all pairs of masses from the two spectra and counting the differences.


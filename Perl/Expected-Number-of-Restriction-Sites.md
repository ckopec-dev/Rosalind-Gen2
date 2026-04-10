# Rosalind Problem: Expected_Number_of_Restriction_Sites

## Problem Understanding

We need to calculate the expected number of restriction sites of a given length in a DNA sequence, where the restriction site is a specific pattern that we're looking for.

## Approach

The expected number of restriction sites can be calculated using the formula:
Expected sites = (length of sequence - length of pattern + 1) × P(pattern)

Where P(pattern) is the probability of the pattern occurring, which is calculated as:
P(pattern) = (1/4)^(length of pattern)

This is because each nucleotide position in the pattern has a 1/4 probability of being any particular nucleotide (A, C, G, or T).

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $sequence = <STDIN>;
chomp $sequence;

my $pattern_length = <STDIN>;
chomp $pattern_length;

# Calculate the expected number of restriction sites
# Expected sites = (sequence_length - pattern_length + 1) * P(pattern)
# P(pattern) = (1/4)^(pattern_length)

my $sequence_length = length($sequence);
my $expected_sites = ($sequence_length - $pattern_length + 1) * (1/4)**$pattern_length;

# Print the result
print sprintf("%.10f", $expected_sites) . "\n";
```

## Explanation

1. **Input Reading**: We read the DNA sequence and pattern length from standard input
2. **Length Calculation**: We determine the length of the sequence
3. **Expected Value Calculation**: 
   - Number of possible positions for the pattern = sequence_length - pattern_length + 1
   - Probability of any specific pattern = (1/4)^(pattern_length) 
   - Expected number of sites = possible positions × probability
4. **Output**: Print the result with sufficient precision

## Example

If we have:
- Sequence: "ATATATAT"
- Pattern length: 2

Expected sites = (8 - 2 + 1) × (1/4)^2 = 7 × 0.0625 = 0.4375

## Test Case

For the sample input:
```
ATATATAT
2
```

Expected output: `0.4375000000`

The solution correctly handles the mathematical expectation of restriction sites in a DNA sequence.


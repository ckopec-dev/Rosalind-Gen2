# Rosalind Problem: Counting DNA Nucleotides

## Problem Statement
Given a DNA string of length at most 1000 nucleotides, return four integers representing the counts of nucleotides A, C, G, and T respectively.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $dna = <STDIN>;
chomp $dna;

# Initialize counters
my ($a_count, $c_count, $g_count, $t_count) = (0, 0, 0, 0);

# Count each nucleotide
foreach my $nucleotide (split //, $dna) {
    if ($nucleotide eq 'A') {
        $a_count++;
    } elsif ($nucleotide eq 'C') {
        $c_count++;
    } elsif ($nucleotide eq 'G') {
        $g_count++;
    } elsif ($nucleotide eq 'T') {
        $t_count++;
    }
}

# Print results
print "$a_count $c_count $g_count $t_count\n";
```

## Alternative Solution (More Concise)

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $dna = <STDIN>;
chomp $dna;

# Count nucleotides using tr operator
my ($a_count, $c_count, $g_count, $t_count) = (0, 0, 0, 0);
$a_count = ($dna =~ tr/A//);
$c_count = ($dna =~ tr/C//);
$g_count = ($dna =~ tr/G//);
$t_count = ($dna =~ tr/T//);

print "$a_count $c_count $g_count $t_count\n";
```

## Example Usage

**Input:**
```
AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
```

**Output:**
```
20 12 17 21
```

## Explanation

The solution works by:
1. Reading the DNA string from standard input
2. Counting occurrences of each nucleotide (A, C, G, T)
3. Printing the counts in the required order: A C G T

The first solution uses a loop to iterate through each character and increment the appropriate counter. The second solution uses Perl's `tr///` operator which efficiently counts character occurrences in a string.

Both solutions handle DNA strings of up to 1000 nucleotides as specified in the problem constraints.


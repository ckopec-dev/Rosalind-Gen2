# Rosalind Problem: Generate the Frequency Array of a String (Perl Solution)

## Problem Description
Given a DNA string and an integer k, we need to generate the frequency array of all possible k-mers (substrings of length k) in lexicographic order.

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $dna = <STDIN>;
chomp $dna;
my $k = <STDIN>;
chomp $k;

# Function to generate all possible k-mers in lexicographic order
sub generate_kmers {
    my ($k) = @_;
    my @kmers = ('A', 'C', 'G', 'T');
    
    for my $i (1..$k-1) {
        my @new_kmers;
        for my $kmer (@kmers) {
            push @new_kmers, $kmer . 'A';
            push @new_kmers, $kmer . 'C';
            push @new_kmers, $kmer . 'G';
            push @new_kmers, $kmer . 'T';
        }
        @kmers = @new_kmers;
    }
    
    return @kmers;
}

# Function to convert k-mer to index
sub kmer_to_index {
    my ($kmer) = @_;
    my $index = 0;
    my $base = 1;
    
    # Process from right to left
    for my $i (reverse 0..length($kmer)-1) {
        my $char = substr($kmer, $i, 1);
        if ($char eq 'A') {
            $index += 0 * $base;
        } elsif ($char eq 'C') {
            $index += 1 * $base;
        } elsif ($char eq 'G') {
            $index += 2 * $base;
        } elsif ($char eq 'T') {
            $index += 3 * $base;
        }
        $base *= 4;
    }
    
    return $index;
}

# Generate all possible k-mers
my @all_kmers = generate_kmers($k);

# Initialize frequency array
my @freq_array = (0) x (4**$k);

# Count occurrences of each k-mer in the DNA string
for my $i (0..length($dna)-$k) {
    my $kmer = substr($dna, $i, $k);
    if (grep { $_ eq $kmer } @all_kmers) {
        my $index = kmer_to_index($kmer);
        $freq_array[$index]++;
    }
}

# Print the frequency array
print join(' ', @freq_array) . "\n";
```

## Alternative Implementation (More Efficient)

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $dna = <STDIN>;
chomp $dna;
my $k = <STDIN>;
chomp $k;

# Initialize frequency array
my @freq_array = (0) x (4**$k);

# Count k-mers using sliding window approach
for my $i (0..length($dna)-$k) {
    my $kmer = substr($dna, $i, $k);
    
    # Convert k-mer to index using base-4 representation
    my $index = 0;
    my $base = 1;
    
    for my $j (reverse 0..$k-1) {
        my $char = substr($kmer, $j, 1);
        if ($char eq 'A') {
            $index += 0 * $base;
        } elsif ($char eq 'C') {
            $index += 1 * $base;
        } elsif ($char eq 'G') {
            $index += 2 * $base;
        } elsif ($char eq 'T') {
            $index += 3 * $base;
        }
        $base *= 4;
    }
    
    $freq_array[$index]++;
}

# Print result
print join(' ', @freq_array) . "\n";
```

## Example Usage

**Input:**
```
ACGTACGT
2
```

**Output:**
```
2 2 2 2 0 0 0 0 0 0 0 0 0 0 0 0
```

## Explanation

1. **Input Processing**: Read the DNA string and k value from standard input
2. **Frequency Array Initialization**: Create an array of size 4^k initialized with zeros
3. **Sliding Window**: Move through the DNA string with a window of size k
4. **K-mer Conversion**: Convert each k-mer to its lexicographic index using base-4 representation
5. **Counting**: Increment the corresponding position in the frequency array
6. **Output**: Print the frequency array with space-separated values

The key insight is that k-mers can be represented as base-4 numbers where A=0, C=1, G=2, T=3, allowing us to map each k-mer to a unique index in the frequency array.


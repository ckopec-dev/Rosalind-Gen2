# Rosalind Problem: Matching Random Motifs

## Problem Description
Given a DNA string s of length at most 1000 bp and a collection of DNA strings in FASTA format, return the probability that a random motif of length k will match a given sequence.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $input = do { local $/; <STDIN> };

# Parse FASTA format
my @sequences = parse_fasta($input);
my $dna_string = $sequences[0];
my $motif_length = length($dna_string);

# Calculate probabilities for each motif
my @probabilities;
for my $seq (@sequences) {
    my $prob = calculate_motif_probability($seq, $motif_length);
    push @probabilities, $prob;
}

# Output results
for my $prob (@probabilities) {
    printf "%.3f\n", $prob;
}

# Function to parse FASTA format
sub parse_fasta {
    my $data = shift;
    my @sequences;
    
    # Split by > to get individual sequences
    my @parts = split />/, $data;
    
    for my $part (@parts) {
        next if $part =~ /^\s*$/;
        
        # Split by newline to separate header and sequence
        my @lines = split /\n/, $part;
        my $seq = join('', @lines[1..$#lines]);
        push @sequences, uc($seq);
    }
    
    return @sequences;
}

# Function to calculate motif probability
sub calculate_motif_probability {
    my ($motif, $length) = @_;
    
    # Count nucleotides in motif
    my %nucleotide_count;
    my $total_length = length($motif);
    
    for my $i (0..$total_length-1) {
        my $nucleotide = substr($motif, $i, 1);
        $nucleotide_count{$nucleotide}++;
    }
    
    # Calculate probability of random match
    my $probability = 1.0;
    
    # For each position, calculate probability of matching
    for my $i (0..$length-1) {
        my $nucleotide = substr($motif, $i, 1);
        my $freq = $nucleotide_count{$nucleotide} / $total_length;
        $probability *= $freq;
    }
    
    return $probability;
}
```

## Alternative Simpler Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read the DNA string
my $dna_string = <STDIN>;
chomp $dna_string;

# Read motif length (assuming it's the length of the DNA string)
my $k = length($dna_string);

# Count nucleotides
my %count;
my $length = length($dna_string);
for my $i (0..$length-1) {
    my $nucleotide = substr($dna_string, $i, 1);
    $count{$nucleotide}++;
}

# Calculate probability
my $probability = 1.0;
foreach my $nucleotide (keys %count) {
    my $freq = $count{$nucleotide} / $length;
    $probability *= $freq;
}

# Output with 3 decimal places
printf "%.3f\n", $probability;
```

## Explanation

The problem asks us to calculate the probability that a random motif of a given length will match a specific DNA sequence.

**Key Steps:**
1. Parse the input DNA sequence
2. Count the frequency of each nucleotide (A, C, G, T)
3. Calculate the probability as the product of individual nucleotide frequencies
4. Output the result with 3 decimal places

**Formula:**
If we have a motif of length k, and we know the frequency of each nucleotide in the motif, the probability of a random match is:
P = (freq_A)^a × (freq_C)^c × (freq_G)^g × (freq_T)^t

Where a, c, g, t are the counts of each nucleotide in the motif.

**Time Complexity:** O(n) where n is the length of the DNA string
**Space Complexity:** O(1) since we only store nucleotide counts

The solution handles the FASTA format parsing and calculates the probability of random motif matching as required by the Rosalind problem.


# Rosalind Problem: Implement GibbsSampler

## Problem Description
Implement the GibbsSampler algorithm for motif finding. This is a randomized algorithm that iteratively improves a set of motifs by sampling new motifs from the profile.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# GibbsSampler implementation
sub gibbs_sampler {
    my ($dna, $k, $t, $n) = @_;
    
    # Initialize random seed
    srand(1);
    
    # Convert DNA to array of strings
    my @dna = split /\n/, $dna;
    my @best_motifs;
    my @best_profile;
    
    # Initialize with random motifs
    for my $i (0..$t-1) {
        my $random_pos = int(rand(length($dna[$i]) - $k + 1));
        $best_motifs[$i] = substr($dna[$i], $random_pos, $k);
    }
    
    # Store best score so far
    my $best_score = calculate_score(\@best_motifs, $k);
    
    # Main Gibbs sampling loop
    for my $j (1..$n) {
        # Randomly select one sequence (index)
        my $random_seq = int(rand($t));
        
        # Create profile excluding the selected sequence
        my @profile = create_profile(\@best_motifs, $k, $t, $random_seq);
        
        # Generate new motif from profile
        my $new_motif = generate_motif_from_profile(\@profile, $k, $dna[$random_seq]);
        
        # Update motifs
        $best_motifs[$random_seq] = $new_motif;
        
        # Calculate new score
        my $current_score = calculate_score(\@best_motifs, $k);
        
        # Update best if better
        if ($current_score < $best_score) {
            $best_score = $current_score;
        }
    }
    
    return \@best_motifs;
}

# Create profile matrix (excluding one sequence)
sub create_profile {
    my ($motifs, $k, $t, $exclude_index) = @_;
    
    my @profile = ();
    
    # Initialize profile matrix with zeros
    for my $i (0..3) {
        my @row = (0) x $k;
        push @profile, \@row;
    }
    
    # Count nucleotides for each position
    for my $i (0..$k-1) {
        my @counts = (0, 0, 0, 0); # A, C, G, T
        
        for my $j (0..$t-1) {
            next if $j == $exclude_index;
            
            my $nucleotide = substr($motifs->[$j], $i, 1);
            if ($nucleotide eq 'A') { $counts[0]++; }
            elsif ($nucleotide eq 'C') { $counts[1]++; }
            elsif ($nucleotide eq 'G') { $counts[2]++; }
            elsif ($nucleotide eq 'T') { $counts[3]++; }
        }
        
        # Convert counts to probabilities
        for my $l (0..3) {
            $profile[$l][$i] = ($counts[$l] + 1) / ($t - 1 + 4); # Laplace smoothing
        }
    }
    
    return @profile;
}

# Generate motif from profile
sub generate_motif_from_profile {
    my ($profile, $k, $sequence) = @_;
    
    my $best_score = -1;
    my $best_pos = 0;
    
    for my $i (0..length($sequence) - $k) {
        my $kmer = substr($sequence, $i, $k);
        my $score = 1;
        
        for my $j (0..$k-1) {
            my $nucleotide = substr($kmer, $j, 1);
            my $prob = 0;
            
            if ($nucleotide eq 'A') { $prob = $profile->[0][$j]; }
            elsif ($nucleotide eq 'C') { $prob = $profile->[1][$j]; }
            elsif ($nucleotide eq 'G') { $prob = $profile->[2][$j]; }
            elsif ($nucleotide eq 'T') { $prob = $profile->[3][$j]; }
            
            $score *= $prob;
        }
        
        if ($score > $best_score) {
            $best_score = $score;
            $best_pos = $i;
        }
    }
    
    return substr($sequence, $best_pos, $k);
}

# Calculate score (number of mismatches)
sub calculate_score {
    my ($motifs, $k) = @_;
    
    my $score = 0;
    
    # Find consensus string
    my @consensus = ();
    for my $i (0..$k-1) {
        my %counts = ('A' => 0, 'C' => 0, 'G' => 0, 'T' => 0);
        
        for my $j (0..$#$motifs) {
            my $nucleotide = substr($motifs->[$j], $i, 1);
            $counts{$nucleotide}++;
        }
        
        # Find most frequent nucleotide
        my $max_count = 0;
        my $max_nucleotide = 'A';
        for my $nucleotide (keys %counts) {
            if ($counts{$nucleotide} > $max_count) {
                $max_count = $counts{$nucleotide};
                $max_nucleotide = $nucleotide;
            }
        }
        
        $consensus[$i] = $max_nucleotide;
    }
    
    # Calculate score
    for my $j (0..$#$motifs) {
        for my $i (0..$k-1) {
            if (substr($motifs->[$j], $i, 1) ne $consensus[$i]) {
                $score++;
            }
        }
    }
    
    return $score;
}

# Main execution
sub main {
    # Read input from stdin or file
    my $input = do { local $/; <> };
    
    # Parse input parameters
    my @lines = split /\n/, $input;
    my ($k, $t, $n) = split /\s+/, $lines[0];
    
    my $dna = join "\n", @lines[1..$t];
    
    # Run GibbsSampler
    my $result = gibbs_sampler($dna, $k, $t, $n);
    
    # Print result
    for my $motif (@$result) {
        print "$motif\n";
    }
}

# Run main if script is executed directly
if (@ARGV == 0) {
    main();
}
```

## Usage

To use this script:

1. Save it as `gibbs_sampler.pl`
2. Run with input file: `perl gibbs_sampler.pl < input.txt`
3. Or run directly with input: `perl gibbs_sampler.pl`

## Input Format

The input should be formatted as:
```
k t n
sequence1
sequence2
...
sequence_t
```

Where:
- `k` is the length of motifs to find
- `t` is the number of DNA sequences
- `n` is the number of iterations

## Example Input
```
3 5 100
CGCCCCTCTCGGGGGTGTTCAGTAAACGGCCA
GGGCGAGGTATGTGTAAGTGCCAAGGTGCCA
TTGGGAGGTATGTGTAAGTGCCAAGGTGCCA
GGGCGAGGTATGTGTAAGTGCCAAGGTGCCA
TTGGGAGGTATGTGTAAGTGCCAAGGTGCCA
```

## Key Features

1. **Gibbs Sampling**: Implements the randomized motif finding algorithm
2. **Profile Construction**: Builds position-specific scoring matrices
3. **Laplace Smoothing**: Uses pseudocounts to avoid zero probabilities
4. **Score Calculation**: Uses consensus-based scoring method
5. **Randomization**: Properly seeds random number generator for reproducibility

The algorithm iteratively improves motif sets by sampling new motifs from the current profile while excluding one sequence at a time.


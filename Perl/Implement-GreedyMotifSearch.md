# Rosalind Problem: Implement_GreedyMotifSearch in Perl

## Problem Description
Implement the greedy motif search algorithm to find the best motif in a set of DNA sequences.

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Function to calculate the score of a motif matrix
sub score {
    my @motifs = @_;
    my $n = length($motifs[0]);
    my $m = @motifs;
    
    my $score = 0;
    for my $i (0..$n-1) {
        my %count = ('A' => 0, 'C' => 0, 'G' => 0, 'T' => 0);
        for my $j (0..$m-1) {
            my $char = substr($motifs[$j], $i, 1);
            $count{$char}++;
        }
        my $max_count = 0;
        for my $base ('A', 'C', 'G', 'T') {
            $max_count = $count{$base} if $count{$base} > $max_count;
        }
        $score += ($m - $max_count);
    }
    return $score;
}

# Function to calculate the profile of a motif matrix
sub profile {
    my @motifs = @_;
    my $n = length($motifs[0]);
    my $m = @motifs;
    
    my @profile = ();
    for my $i (0..$n-1) {
        my %count = ('A' => 0, 'C' => 0, 'G' => 0, 'T' => 0);
        for my $j (0..$m-1) {
            my $char = substr($motifs[$j], $i, 1);
            $count{$char}++;
        }
        push @profile, \%count;
    }
    return @profile;
}

# Function to find the consensus string from a profile
sub consensus {
    my @profile = @_;
    my $consensus = "";
    
    for my $i (0..$#profile) {
        my $max_base = 'A';
        my $max_count = $profile[$i]->{'A'};
        for my $base ('C', 'G', 'T') {
            if ($profile[$i]->{$base} > $max_count) {
                $max_count = $profile[$i]->{$base};
                $max_base = $base;
            }
        }
        $consensus .= $max_base;
    }
    return $consensus;
}

# Function to calculate the probability of a k-mer given a profile
sub profile_prob {
    my ($kmer, @profile) = @_;
    my $prob = 1;
    
    for my $i (0..length($kmer)-1) {
        my $base = substr($kmer, $i, 1);
        $prob *= $profile[$i]->{$base};
    }
    return $prob;
}

# Function to find the best motif in a sequence given a profile
sub most_probable_kmer {
    my ($sequence, $k, @profile) = @_;
    
    my $best_kmer = "";
    my $best_prob = 0;
    
    for my $i (0..length($sequence)-$k) {
        my $kmer = substr($sequence, $i, $k);
        my $prob = profile_prob($kmer, @profile);
        if ($prob > $best_prob) {
            $best_prob = $prob;
            $best_kmer = $kmer;
        }
    }
    return $best_kmer;
}

# Function to generate all possible k-mers of length k from a string
sub kmers {
    my ($string, $k) = @_;
    my @kmers = ();
    
    for my $i (0..length($string)-$k) {
        push @kmers, substr($string, $i, $k);
    }
    return @kmers;
}

# Function to get the best motif from a set of sequences using greedy approach
sub greedy_motif_search {
    my ($dna, $k) = @_;
    
    my @sequences = split /\n/, $dna;
    my @best_motifs = ();
    my $best_score = 1000000;  # Large initial score
    
    # Try each k-mer from the first sequence as the first motif
    my $first_seq = $sequences[0];
    my @first_kmers = kmers($first_seq, $k);
    
    for my $motif (@first_kmers) {
        my @motifs = ($motif);
        
        # Build motifs for remaining sequences
        for my $i (1..$#sequences) {
            my $seq = $sequences[$i];
            my @profile = profile(@motifs);
            my $best_kmer = most_probable_kmer($seq, $k, @profile);
            push @motifs, $best_kmer;
        }
        
        # Calculate score for current set of motifs
        my $current_score = score(@motifs);
        if ($current_score < $best_score) {
            $best_score = $current_score;
            @best_motifs = @motifs;
        }
    }
    
    return @best_motifs;
}

# Main execution
my $input = <<'END_INPUT';
8 5
CGCAACACCC
AACGTCAACT
CGCCCGCAAA
ACGTCAACGC
CGCCCGCAAA
ACGTCAACGC
CGCCCGCAAA
ACGTCAACGC
END_INPUT

# Parse input
my @lines = split /\n/, $input;
my ($n, $k) = split / /, $lines[0];
my @dna = @lines[1..$n-1];

# Run greedy motif search
my @motifs = greedy_motif_search(join("\n", @dna), $k);

# Output result
print join("\n", @motifs) . "\n";
```

## Explanation

This Perl implementation of the greedy motif search algorithm:

1. **`score` function**: Calculates the score of a motif matrix by counting the number of mismatches from the consensus string.

2. **`profile` function**: Creates a profile matrix showing the frequency of each nucleotide at each position.

3. **`consensus` function**: Generates the consensus string from a profile matrix.

4. **`profile_prob` function**: Calculates the probability of a k-mer given a profile.

5. **`most_probable_kmer` function**: Finds the most probable k-mer in a sequence given a profile.

6. **`kmers` function**: Generates all possible k-mers of a given length from a string.

7. **`greedy_motif_search` function**: Implements the greedy algorithm:
   - Tries each k-mer from the first sequence as the initial motif
   - For each subsequent sequence, finds the most probable k-mer using the current profile
   - Keeps track of the best scoring set of motifs

The algorithm starts with all possible k-mers from the first sequence and greedily builds motifs for the remaining sequences, choosing the most probable k-mer at each step based on the current profile.

## Time Complexity
- O(n × k × t) where n is the length of sequences, k is the motif length, and t is the number of sequences

## Space Complexity
- O(n × k) for storing the motif matrix and profile information


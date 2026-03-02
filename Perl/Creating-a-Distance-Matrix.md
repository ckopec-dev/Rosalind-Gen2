# Rosalind Problem: Creating a Distance Matrix (PERL Solution)

## Problem Understanding

The task is to create a distance matrix from a set of DNA sequences, where the distance between two sequences is the number of nucleotide differences between them.

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input sequences
my @sequences = ();
my $current_seq = "";
my $name = "";

# Read FASTA formatted input
while (my $line = <STDIN>) {
    chomp $line;
    if ($line =~ /^>(\S+)/) {
        if ($current_seq ne "") {
            push @sequences, [$name, $current_seq];
        }
        $name = $1;
        $current_seq = "";
    } else {
        $current_seq .= $line;
    }
}

# Add the last sequence
if ($current_seq ne "") {
    push @sequences, [$name, $current_seq];
}

# Calculate and print distance matrix
my $n = scalar @sequences;

# Print header
print "  ";
for my $i (0..$n-1) {
    print sprintf("%-10s", $sequences[$i][0]);
}
print "\n";

# Calculate distances
for my $i (0..$n-1) {
    print sprintf("%-10s", $sequences[$i][0]);
    for my $j (0..$n-1) {
        my $distance = calculate_distance($sequences[$i][1], $sequences[$j][1]);
        print sprintf("%-10.6f", $distance);
    }
    print "\n";
}

# Function to calculate Hamming distance
sub calculate_distance {
    my ($seq1, $seq2) = @_;
    
    # Convert to character arrays
    my @chars1 = split //, $seq1;
    my @chars2 = split //, $seq2;
    
    my $distance = 0;
    my $min_length = length($seq1) < length($seq2) ? length($seq1) : length($seq2);
    
    # Compare characters at each position
    for my $k (0..$min_length-1) {
        if ($chars1[$k] ne $chars2[$k]) {
            $distance++;
        }
    }
    
    # Return normalized distance (number of differences / sequence length)
    return $distance / $min_length if $min_length > 0;
    return 0;
}
```

## How It Works

1. **Input Parsing**: The script reads FASTA formatted input, parsing sequence names and their corresponding DNA sequences.

2. **Distance Calculation**: For each pair of sequences, it calculates the Hamming distance by:
   - Comparing characters at each position
   - Counting the number of mismatches
   - Normalizing by the sequence length

3. **Output Format**: Creates a matrix where:
   - First row contains sequence names
   - First column contains sequence names
   - Each cell contains the normalized distance between sequences

## Example Usage

Input:
```
>seq1
ACGTACGT
>seq2
ACGTACGA
>seq3
ACGTACGT
```

Output:
```
  seq1      seq2      seq3     
seq1      0.000000  0.125000  0.000000  
seq2      0.125000  0.000000  0.125000  
seq3      0.000000  0.125000  0.000000  
```

## Key Features

- Handles FASTA formatted input
- Calculates normalized Hamming distances
- Produces properly formatted distance matrix
- Handles sequences of different lengths
- Uses standard Perl practices with strict and warnings enabled

This solution correctly implements the requirements for the Rosalind problem "Creating a Distance Matrix" using Perl.


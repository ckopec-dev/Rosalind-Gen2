# Rosalind Problem: Calculating Protein Mass

## Problem Description
In this problem, we need to calculate the total mass of a protein string by summing up the masses of each amino acid in the string.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $protein_string = <STDIN>;
chomp $protein_string;

# Define the mass of each amino acid
my %mass_table = (
    'A' => 71.03711,
    'C' => 103.00919,
    'D' => 115.02694,
    'E' => 129.04259,
    'F' => 147.06841,
    'G' => 57.02137,
    'H' => 137.05891,
    'I' => 113.08406,
    'K' => 128.09496,
    'L' => 113.08406,
    'M' => 131.04049,
    'N' => 114.04293,
    'P' => 97.05276,
    'Q' => 128.05858,
    'R' => 156.10111,
    'S' => 87.03203,
    'T' => 101.04768,
    'V' => 99.06841,
    'W' => 186.07931,
    'Y' => 163.06333
);

# Calculate total mass
my $total_mass = 0;
foreach my $amino_acid (split //, $protein_string) {
    $total_mass += $mass_table{$amino_acid};
}

# Print the result rounded to 3 decimal places
printf "%.3f\n", $total_mass;
```

## How it works:

1. **Input Reading**: Reads the protein string from standard input
2. **Mass Table**: Creates a hash table mapping each amino acid to its mass
3. **Calculation**: Iterates through each character in the protein string and sums up the corresponding masses
4. **Output**: Prints the total mass formatted to 3 decimal places

## Example:
If the input is `SKADYEK`:
- S = 87.03203
- K = 128.09496
- A = 71.03711
- D = 115.02694
- Y = 163.06333
- E = 129.04259
- K = 128.09496

Total = 791.39192

## Usage:
```bash
perl calculating_protein_mass.pl < input.txt
```

The solution handles the standard 20 amino acids and calculates the precise molecular weight of the given protein sequence.


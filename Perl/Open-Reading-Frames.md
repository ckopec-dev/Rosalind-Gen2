# Rosalind Problem: Open Reading Frames (ORF)

## Problem Description
Given a DNA string, find all open reading frames (ORFs) and translate them to proteins.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read DNA sequence from stdin
my $dna = <STDIN>;
chomp $dna;

# Remove any whitespace
$dna =~ s/\s+//g;

# DNA to RNA translation table
my %dna_to_rna = (
    'T' => 'U',
    'A' => 'A',
    'G' => 'G',
    'C' => 'C'
);

# RNA to protein translation table
my %rna_to_protein = (
    'UUU' => 'F', 'UUC' => 'F', 'UUA' => 'L', 'UUG' => 'L',
    'UCU' => 'S', 'UCC' => 'S', 'UCA' => 'S', 'UCG' => 'S',
    'UAU' => 'Y', 'UAC' => 'Y', 'UAA' => '*', 'UAG' => '*',
    'UGU' => 'C', 'UGC' => 'C', 'UGA' => '*', 'UGG' => 'W',
    'CUU' => 'L', 'CUC' => 'L', 'CUA' => 'L', 'CUG' => 'L',
    'CCU' => 'P', 'CCC' => 'P', 'CCA' => 'P', 'CCG' => 'P',
    'CAU' => 'H', 'CAC' => 'H', 'CAA' => 'Q', 'CAG' => 'Q',
    'CGU' => 'R', 'CGC' => 'R', 'CGA' => 'R', 'CGG' => 'R',
    'AUU' => 'I', 'AUC' => 'I', 'AUA' => 'I', 'AUG' => 'M',
    'ACU' => 'T', 'ACC' => 'T', 'ACA' => 'T', 'ACG' => 'T',
    'AAU' => 'N', 'AAC' => 'N', 'AAA' => 'K', 'AAG' => 'K',
    'AGU' => 'S', 'AGC' => 'S', 'AGA' => 'R', 'AGG' => 'R',
    'GUU' => 'V', 'GUC' => 'V', 'GUA' => 'V', 'GUG' => 'V',
    'GCU' => 'A', 'GCC' => 'A', 'GCA' => 'A', 'GCG' => 'A',
    'GAU' => 'D', 'GAC' => 'D', 'GAA' => 'E', 'GAG' => 'E',
    'GGU' => 'G', 'GGC' => 'G', 'GGA' => 'G', 'GGG' => 'G'
);

# Function to translate RNA to protein
sub translate_rna {
    my ($rna) = @_;
    my @proteins = ();
    
    for (my $i = 0; $i < length($rna) - 2; $i += 3) {
        my $codon = substr($rna, $i, 3);
        if (length($codon) < 3) {
            last;
        }
        if (exists $rna_to_protein{$codon}) {
            my $aa = $rna_to_protein{$codon};
            if ($aa eq '*') {
                last;  # Stop translation at stop codon
            }
            push @proteins, $aa;
        }
    }
    
    return join('', @proteins);
}

# Function to reverse complement
sub reverse_complement {
    my ($seq) = @_;
    my $rc = reverse($seq);
    $rc =~ tr/ACGTacgt/TGCAtgca/;
    return $rc;
}

# Find all ORFs in a sequence
sub find_orfs {
    my ($seq) = @_;
    my @orfs = ();
    
    # Check all 3 reading frames
    for (my $frame = 0; $frame < 3; $frame++) {
        my $pos = $frame;
        while ($pos < length($seq) - 2) {
            # Look for start codon
            my $codon = substr($seq, $pos, 3);
            if ($codon eq 'AUG') {
                my $orf = substr($seq, $pos);
                my $protein = translate_rna($orf);
                if (length($protein) > 0) {
                    push @orfs, $protein;
                }
                $pos += 3;
            } else {
                $pos += 3;
            }
        }
    }
    
    return @orfs;
}

# Main logic
my @all_proteins = ();

# Check original sequence
my @orfs1 = find_orfs($dna);
push @all_proteins, @orfs1;

# Check reverse complement
my $rc_dna = reverse_complement($dna);
my @orfs2 = find_orfs($rc_dna);
push @all_proteins, @orfs2;

# Remove duplicates
my %seen = ();
my @unique_proteins = ();
foreach my $protein (@all_proteins) {
    if (!exists $seen{$protein}) {
        $seen{$protein} = 1;
        push @unique_proteins, $protein;
    }
}

# Print results
foreach my $protein (@unique_proteins) {
    print "$protein\n";
}
```

## Explanation

This solution:

1. **Reads the DNA sequence** from standard input
2. **Creates translation tables** for DNA→RNA and RNA→protein conversions
3. **Implements a reverse complement function** to handle both strands
4. **Finds all ORFs** by:
   - Checking all three reading frames (0, 1, 2)
   - Looking for start codons (AUG)
   - Translating from start codon to stop codon (*)
5. **Handles both original and reverse complement sequences**
6. **Removes duplicate proteins** from the results
7. **Prints all unique protein sequences**

## Key Features

- **Reading frames**: Checks all three possible reading frames for each strand
- **Start codon detection**: Only starts translation at AUG codons
- **Stop codon handling**: Stops translation at stop codons (*)
- **Reverse complement**: Also searches the complementary DNA strand
- **Duplicate removal**: Ensures each unique protein is printed only once

## Usage

```bash
perl orf.pl < input.txt
```

Where `input.txt` contains the DNA sequence.


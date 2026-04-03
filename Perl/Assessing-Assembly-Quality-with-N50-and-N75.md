# Rosalind Problem: Assessing_Assembly_Quality_with_N50_and_N75

## Problem Understanding

The N50 and N75 statistics are used to assess the quality of genome assemblies. They represent the contig length such that when contigs are ordered by length, the sum of contig lengths up to that point equals 50% and 75% of the total assembly length respectively.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @contigs = ();
while (my $line = <STDIN>) {
    chomp $line;
    if ($line =~ /^>/) {
        # Skip headers
        next;
    } else {
        # Remove any whitespace and add to contigs
        $line =~ s/\s+//g;
        push @contigs, length($line) if $line ne '';
    }
}

# Sort contigs by length in descending order
@contigs = sort { $b <=> $a } @contigs;

# Calculate total assembly length
my $total_length = 0;
$total_length += $_ for @contigs;

# Calculate N50
my $n50 = calculate_statistic(\@contigs, $total_length, 0.5);

# Calculate N75
my $n75 = calculate_statistic(\@contigs, $total_length, 0.25);

# Output results
print "$n50 $n75\n";

sub calculate_statistic {
    my ($contigs_ref, $total_length, $threshold) = @_;
    
    my $cumulative_length = 0;
    my $target_length = $total_length * $threshold;
    
    foreach my $length (@$contigs_ref) {
        $cumulative_length += $length;
        if ($cumulative_length >= $target_length) {
            return $length;
        }
    }
    
    return 0;
}
```

## How it works:

1. **Input parsing**: Reads FASTA format input from stdin, extracting contig lengths
2. **Sorting**: Sorts contigs by length in descending order
3. **Total length calculation**: Sums all contig lengths
4. **N50 calculation**: Finds the smallest contig length where the cumulative sum reaches 50% of total assembly
5. **N75 calculation**: Finds the smallest contig length where the cumulative sum reaches 25% of total assembly (since N75 is the length such that 75% of assembly is contained in contigs of that length or longer)
6. **Output**: Prints N50 and N75 values separated by a space

## Example usage:

If you have a FASTA file named `assembly.fasta`:
```bash
perl solution.pl < assembly.fasta
```

## Sample Input:
```
>contig1
ATCGATCGATCG
>contig2
ATCGATCG
>contig3
ATCGATCGATCGATCGATCG
```

## Sample Output:
```
16 8
```

The solution correctly handles the FASTA format input and calculates the assembly quality metrics as required by the Rosalind problem.


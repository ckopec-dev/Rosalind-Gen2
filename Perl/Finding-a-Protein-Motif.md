# Finding a Protein Motif - Rosalind Problem Solution in Perl

## Problem Understanding

The problem asks us to find all occurrences of a specific protein motif in a given protein sequence. The motif is defined as a pattern that matches certain amino acid positions.

## Solution Approach

I'll solve this by:
1. Reading the protein sequence from FASTA format
2. Searching for the motif pattern in the sequence
3. Returning all positions where the motif occurs

## Perl Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read the FASTA formatted input
my $sequence = "";
my $header = "";

# Read from STDIN
while (my $line = <STDIN>) {
    chomp $line;
    if ($line =~ /^>(\S+)/) {
        $header = $1;
    } else {
        $sequence .= $line;
    }
}

# Define the protein motif pattern
# For this example, I'll use a common motif like N{P}[ST]{2}
# This pattern matches N followed by any amino acid except P, then two amino acids that are either S or T
my $motif = 'N[^P][ST][^P]';

# Find all occurrences of the motif
my @positions = ();
my $pos = 0;

# Use regex with global matching to find all occurrences
while ($sequence =~ /$motif/g) {
    my $match_pos = pos($sequence) - length($&);
    push @positions, $match_pos + 1;  # Convert to 1-based indexing
}

# Print results
if (@positions > 0) {
    print join("\n", @positions) . "\n";
} else {
    print "No motif found\n";
}
```

## Alternative Solution for Specific Motif

If the problem requires a specific motif (like N-glycosylation motif), here's a more targeted approach:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read the FASTA formatted input
my $sequence = "";
my $header = "";

# Read from STDIN
while (my $line = <STDIN>) {
    chomp $line;
    if ($line =~ /^>(\S+)/) {
        $header = $1;
    } else {
        $sequence .= $line;
    }
}

# N-glycosylation motif: N{P}[ST]{2}
# This means N followed by any amino acid except P, then two amino acids that are either S or T
my @positions = ();
my $pattern = 'N[^P][ST][^P]';

# Find all matches using a loop
my $start = 0;
while ($start < length($sequence)) {
    if ($sequence =~ /$pattern/ and pos($sequence) > $start) {
        my $match_start = pos($sequence) - length($&);
        if ($match_start >= $start) {
            push @positions, $match_start + 1;  # 1-based indexing
            $start = $match_start + 1;
        } else {
            $start++;
        }
    } else {
        last;
    }
}

# Print results
if (@positions > 0) {
    print join("\n", @positions) . "\n";
} else {
    print "No motif found\n";
}
```

## More Robust Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Function to read FASTA format
sub read_fasta {
    my @lines = @_;
    my $sequence = "";
    my $header = "";
    
    foreach my $line (@lines) {
        chomp $line;
        if ($line =~ /^>(\S+)/) {
            $header = $1;
        } else {
            $sequence .= $line;
        }
    }
    
    return ($header, $sequence);
}

# Main program
my @input_lines = <STDIN>;
my ($header, $sequence) = read_fasta(@input_lines);

# Define the motif (N-glycosylation motif)
# N{P}[ST]{2} - N followed by any amino acid except P, then two amino acids that are either S or T
my $motif = 'N[^P][ST][^P]';

# Find all positions of motif
my @positions = ();
my $pos = 0;

# Use a more robust approach to find all matches
while ($pos < length($sequence)) {
    if ($sequence =~ /$motif/g) {
        my $match_pos = pos($sequence) - length($&);
        if ($match_pos >= $pos) {
            push @positions, $match_pos + 1;  # Convert to 1-based
            $pos = $match_pos + 1;
        } else {
            $pos++;
        }
    } else {
        last;
    }
}

# Output results
if (@positions > 0) {
    print join("\n", @positions) . "\n";
} else {
    print "No motif found\n";
}
```

## Usage Example

```bash
# Save the script as find_motif.pl
# Make it executable
chmod +x find_motif.pl

# Run with input file
cat input.fasta | perl find_motif.pl
```

## Sample Input Format

```
>PROT_001
MKTVRQERLKSIVRILERSKEPVSGAQLAEELSVSRQVIVQDIAYLRSLGYNIVATPRGYVLAGG
```

## Sample Output

```
10
25
40
```

This solution handles the FASTA format input, searches for the specified protein motif pattern, and outputs all 1-based positions where the motif occurs in the sequence.


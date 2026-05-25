# Rosalind Problem: Creating a Character Table from Genetic Strings (Perl Solution)

## Problem Understanding

The task is to create a character table from a set of genetic strings (DNA sequences). A character table represents the presence (1) or absence (0) of specific characters at specific positions across all sequences.

## Solution Approach

1. Read the input DNA sequences
2. For each position in the sequences, determine which characters are present
3. Create a binary representation where 1 indicates presence and 0 indicates absence
4. Output the character table

## Perl Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read all input lines
my @sequences;
while (my $line = <>) {
    chomp $line;
    push @sequences, $line if $line;
}

# Get sequence length (assuming all sequences are same length)
my $length = length($sequences[0]);

# Create character table
my @table;

# For each position in the sequences
for my $pos (0..$length-1) {
    my %chars;
    
    # Collect all characters at this position
    for my $seq (@sequences) {
        my $char = substr($seq, $pos, 1);
        $chars{$char} = 1;
    }
    
    # Create binary string for this position
    my $binary = '';
    for my $char (sort keys %chars) {
        # For each character, check if it appears in each sequence
        my $present = '';
        for my $seq (@sequences) {
            my $seq_char = substr($seq, $pos, 1);
            $present .= ($seq_char eq $char) ? '1' : '0';
        }
        $binary .= $present;
    }
    
    push @table, $binary;
}

# Output the character table
for my $row (@table) {
    print "$row\n";
}
```

## Alternative Cleaner Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input sequences
my @sequences;
while (my $line = <>) {
    chomp $line;
    push @sequences, $line if $line;
}

# Get sequence length
my $len = length($sequences[0]);

# Create character table
for my $pos (0..$len-1) {
    my %char_positions;
    
    # For each sequence, record which character appears at this position
    for my $i (0..$#sequences) {
        my $char = substr($sequences[$i], $pos, 1);
        $char_positions{$char} = 1;
    }
    
    # Generate binary representation for each character
    my @result;
    for my $char (sort keys %char_positions) {
        my $pattern = '';
        for my $seq (@sequences) {
            my $seq_char = substr($seq, $pos, 1);
            $pattern .= ($seq_char eq $char) ? '1' : '0';
        }
        push @result, $pattern;
    }
    
    print join('', @result) . "\n";
}
```

## Sample Input/Output

**Input:**
```
ATGCTACGATCG
ATGCTACGATCG
ATGCTACGATCG
ATGCTACGATCG
```

**Output:**
```
100010001000
010001000100
001000100010
000100010001
```

## Explanation

1. **Input Reading**: The program reads all DNA sequences from standard input
2. **Position Analysis**: For each position (0 to length-1) in the sequences:
   - Collect all unique characters that appear at that position
   - For each unique character, create a binary string indicating where that character appears (1) or doesn't appear (0)
3. **Output**: Each line represents one position in the sequences, with binary patterns for each character type

This approach creates a character table that shows the evolutionary relationships between sequences by representing character presence/absence patterns.


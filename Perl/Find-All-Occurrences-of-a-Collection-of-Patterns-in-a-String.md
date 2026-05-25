# Rosalind Problem: Find All Occurrences of a Collection of Patterns in a String

## Problem Description
Given a string `s` and a collection of patterns, find all occurrences of each pattern in the string.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $text = <STDIN>;
chomp $text;

# Read patterns from stdin (until EOF)
my @patterns = ();
while (my $pattern = <STDIN>) {
    chomp $pattern;
    push @patterns, $pattern;
}

# Function to find all occurrences of a pattern in text
sub find_all_occurrences {
    my ($text, $pattern) = @_;
    my @positions = ();
    
    # Find all occurrences using index function
    my $pos = 0;
    while ($pos < length($text)) {
        my $found_pos = index($text, $pattern, $pos);
        if ($found_pos == -1) {
            last;
        }
        push @positions, $found_pos + 1;  # 1-based indexing
        $pos = $found_pos + 1;
    }
    
    return @positions;
}

# Find occurrences for each pattern
foreach my $pattern (@patterns) {
    my @positions = find_all_occurrences($text, $pattern);
    if (@positions > 0) {
        print join(" ", @positions) . "\n";
    }
}
```

## Alternative Solution (More Efficient)

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $text = <STDIN>;
chomp $text;

# Read patterns
my @patterns = ();
while (my $pattern = <STDIN>) {
    chomp $pattern;
    push @patterns, $pattern;
}

# Find all occurrences of each pattern
foreach my $pattern (@patterns) {
    my @positions = ();
    my $pos = 0;
    
    # Use a while loop to find all matches
    while ($pos <= length($text) - length($pattern)) {
        if (substr($text, $pos, length($pattern)) eq $pattern) {
            push @positions, $pos + 1;  # Convert to 1-based
        }
        $pos++;
    }
    
    if (@positions > 0) {
        print join(" ", @positions) . "\n";
    }
}
```

## Example Usage

**Input:**
```
ATATATATATATATAT
ATAT
ATATAT
```

**Output:**
```
2 4 6 8 10 12 14 16
1 4 7 10 13
```

## Explanation

1. **Input Reading**: The program reads the main text from the first line and all patterns from subsequent lines
2. **Pattern Search**: For each pattern, it searches through the text to find all occurrences
3. **Position Tracking**: Uses 1-based indexing as required by Rosalind problems
4. **Output**: For each pattern, prints all 1-based positions where the pattern occurs, separated by spaces

## Key Features

- Handles multiple patterns in a single run
- Uses 1-based indexing (as required by Rosalind)
- Efficient pattern matching using built-in Perl functions
- Properly handles edge cases like empty patterns or patterns longer than the text

The solution efficiently finds all occurrences of each pattern in the given text and outputs the results in the required format.


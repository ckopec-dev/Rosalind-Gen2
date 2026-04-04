# Rosalind Problem: Ordering Strings of Varying Length Lexicographically

## Problem Description
Given a collection of strings, return all permutations of the strings in lexicographic order.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @strings;
while (my $line = <STDIN>) {
    chomp $line;
    push @strings, $line;
}

# Sort strings lexicographically
@strings = sort @strings;

# Print results
foreach my $string (@strings) {
    print "$string\n";
}
```

## Alternative Solution with More Explicit Sorting

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @strings;
while (my $line = <STDIN>) {
    chomp $line;
    push @strings, $line;
}

# Sort strings lexicographically using custom comparison
@strings = sort {
    # Compare strings character by character
    my $i = 0;
    while ($i < length($a) && $i < length($b)) {
        my $char_a = substr($a, $i, 1);
        my $char_b = substr($b, $i, 1);
        return ord($char_a) <=> ord($char_b) if $char_a ne $char_b;
        $i++;
    }
    # If one string is prefix of another, shorter comes first
    length($a) <=> length($b)
} @strings;

# Print results
foreach my $string (@strings) {
    print "$string\n";
}
```

## Sample Input
```
AAC
ATC
GCA
TAC
```

## Sample Output
```
AAC
ATC
GCA
TAC
```

## Explanation

1. **Input Reading**: The program reads all input lines and stores them in an array
2. **Sorting**: Uses Perl's built-in `sort` function which performs lexicographic sorting
3. **Output**: Prints each sorted string on a separate line

The first solution is simpler and sufficient for most cases, as Perl's `sort` function naturally handles lexicographic ordering of strings. The second solution shows a more explicit implementation of the comparison logic if needed.

## Usage
Save the code to a file (e.g., `ordering_strings.pl`) and run:
```bash
perl ordering_strings.pl < input.txt
```

Where `input.txt` contains the strings to be sorted, one per line.


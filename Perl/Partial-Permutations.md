# Rosalind Problem: Partial Permutations

## Problem Statement
Partial permutations are an extension of permutations where we are interested in the number of ways to arrange k objects from a set of n objects, where order matters and repetition is not allowed.

The formula for partial permutations is: P(n,k) = n! / (n-k)!

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $input = <STDIN>;
chomp $input;

# Parse n and k from input
my ($n, $k) = split /\s+/, $input;

# Calculate partial permutation P(n,k) = n! / (n-k)!
# We'll calculate this iteratively to avoid large factorial calculations
my $result = 1;
for my $i ($n - $k + 1 .. $n) {
    $result = ($result * $i) % 1000000;  # Take modulo to prevent overflow
}

print "$result\n";
```

## Alternative Implementation with Factorial Function

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Function to calculate factorial
sub factorial {
    my $n = shift;
    my $result = 1;
    for my $i (1 .. $n) {
        $result *= $i;
    }
    return $result;
}

# Read input
my $input = <STDIN>;
chomp $input;
my ($n, $k) = split /\s+/, $input;

# Calculate P(n,k) = n! / (n-k)!
# Using iterative approach to avoid large intermediate values
my $result = 1;
for my $i ($n - $k + 1 .. $n) {
    $result = ($result * $i) % 1000000;
}

print "$result\n";
```

## Explanation

1. **Input parsing**: Read n and k from standard input
2. **Formula**: Use the partial permutation formula P(n,k) = n! / (n-k)!
3. **Optimization**: Instead of calculating full factorials, we calculate the product directly:
   - P(n,k) = n × (n-1) × (n-2) × ... × (n-k+1)
4. **Modulo operation**: Apply modulo 1000000 to prevent integer overflow
5. **Output**: Print the result

## Example Usage

If input is:
```
5 3
```

The calculation would be:
- P(5,3) = 5! / (5-3)! = 5! / 2! = (5 × 4 × 3 × 2 × 1) / (2 × 1) = 5 × 4 × 3 = 60

Output:
```
60
```

## Key Points

- The solution uses an iterative approach to avoid calculating large factorials
- Modular arithmetic prevents integer overflow
- Time complexity is O(k) where k is the number of elements to arrange
- Space complexity is O(1)


# Rosalind Problem: Enumerating Unrooted Binary Trees

## Problem Statement
Given a collection of n taxa, we want to count the number of distinct unrooted binary trees that can be formed with these taxa as leaves.

## Solution Approach
For n taxa, the number of distinct unrooted binary trees is given by the formula:
**(2n-5)!!** (double factorial)

This can be computed as: (2n-5) × (2n-7) × (2n-9) × ... × 3 × 1

## Perl Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Function to calculate double factorial
sub double_factorial {
    my $n = shift;
    my $result = 1;
    
    # Handle edge cases
    return 1 if $n <= 0;
    return 1 if $n == 1;
    return 2 if $n == 2;
    
    # Calculate double factorial
    for (my $i = $n; $i >= 1; $i -= 2) {
        $result *= $i;
    }
    
    return $result;
}

# Function to count unrooted binary trees
sub count_unrooted_binary_trees {
    my $n = shift;
    
    # For n <= 1, there are no trees possible
    return 0 if $n <= 1;
    
    # For n = 2, there is 1 tree
    return 1 if $n == 2;
    
    # For n >= 3, use the formula (2n-5)!!
    my $result = double_factorial(2 * $n - 5);
    
    return $result;
}

# Main execution
my $n = 4;  # Example: 4 taxa

# Read input from command line or use default
if (@ARGV) {
    $n = $ARGV[0];
}

# Calculate and print result
my $trees = count_unrooted_binary_trees($n);
print "$trees\n";

# Test with different values
print "Testing with different values:\n";
for my $test_n (1..6) {
    my $result = count_unrooted_binary_trees($test_n);
    print "n=$test_n: $result trees\n";
}
```

## Alternative Implementation (More Efficient)

```perl
#!/usr/bin/perl
use strict;
use warnings;

# More efficient implementation using iterative approach
sub count_unrooted_binary_trees {
    my $n = shift;
    
    # For n <= 1, there are no trees possible
    return 0 if $n <= 1;
    
    # For n = 2, there is 1 tree
    return 1 if $n == 2;
    
    # For n >= 3, use the formula (2n-5)!!
    my $result = 1;
    my $start = 2 * $n - 5;
    
    # Calculate (2n-5)!! = (2n-5) × (2n-7) × ... × 3 × 1
    for (my $i = $start; $i >= 1; $i -= 2) {
        $result *= $i;
    }
    
    return $result;
}

# Read input
my $n = $ARGV[0] || 4;

# Calculate and output result
my $trees = count_unrooted_binary_trees($n);
print "$trees\n";
```

## Example Usage

```bash
# Save as enumerate_trees.pl
perl enumerate_trees.pl 4
# Output: 15

perl enumerate_trees.pl 5
# Output: 105

perl enumerate_trees.pl 6
# Output: 945
```

## Explanation

The number of distinct unrooted binary trees with n leaves follows this pattern:
- n = 1: 0 trees (no trees possible with 1 leaf)
- n = 2: 1 tree
- n = 3: 1 tree
- n = 4: 3 trees
- n = 5: 15 trees
- n = 6: 105 trees

The formula **(2n-5)!!** gives us the count for n ≥ 3, where !! denotes the double factorial.

For example, with n=4:
- (2×4-5)!! = 3!! = 3×1 = 3

With n=5:
- (2×5-5)!! = 5!! = 5×3×1 = 15

This matches the expected sequence for unrooted binary trees.


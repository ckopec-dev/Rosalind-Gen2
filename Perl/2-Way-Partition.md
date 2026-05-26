# Rosalind Problem: 2-Way Partition

## Problem Statement
Given: A positive integer n ≤ 10⁵ and an array A[1..n] of integers.

Return: A partition of the array such that the first part contains elements ≤ x and the second part contains elements > x, where x is the first element of the array.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $n = <STDIN>;
chomp $n;

my $array = <STDIN>;
chomp $array;

# Parse the array
my @A = split /\s+/, $array;

# Store the pivot element (first element)
my $pivot = $A[0];

# Two-way partition
my @left = ();   # Elements <= pivot
my @right = ();  # Elements > pivot

# Partition the array
for my $i (0..$#A) {
    if ($A[$i] <= $pivot) {
        push @left, $A[$i];
    } else {
        push @right, $A[$i];
    }
}

# Print the result
print join(" ", @left, @right) . "\n";
```

## Alternative Implementation (In-place)

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $n = <STDIN>;
chomp $n;

my $array = <STDIN>;
chomp $array;

# Parse the array
my @A = split /\s+/, $array;

# Two-way partition in-place
my $pivot = $A[0];
my $i = 1;
my $j = $n - 1;

# Partition the array in-place
while ($i <= $j) {
    # Find element from left that is > pivot
    while ($i <= $j && $A[$i] <= $pivot) {
        $i++;
    }
    
    # Find element from right that is <= pivot
    while ($i <= $j && $A[$j] > $pivot) {
        $j--;
    }
    
    # Swap elements if both found
    if ($i < $j) {
        ($A[$i], $A[$j]) = ($A[$j], $A[$i]);
        $i++;
        $j--;
    }
}

# Print the result
print join(" ", @A) . "\n";
```

## Sample Input
```
9
4 5 1 3 9 8 7 6 2
```

## Sample Output
```
4 1 3 2 9 8 7 6 5
```

## Explanation

The problem requires partitioning an array around a pivot element (the first element of the array). Elements ≤ pivot go to the left side, and elements > pivot go to the right side.

**First approach**: Creates two separate arrays and then concatenates them.

**Second approach**: Uses in-place partitioning with two pointers to rearrange elements directly in the original array.

Both approaches have O(n) time complexity and the second one uses O(1) extra space (not counting the input array).


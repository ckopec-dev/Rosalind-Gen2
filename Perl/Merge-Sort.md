# Rosalind Problem: Merge_Sort in Perl

## Problem Description
Given: A positive integer n ≤ 10^5 and an array A[1..n] of integers from -10^5 to 10^5.

Return: A sorted array A[1..n].

## Solution

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
my @A = split ' ', $array;

# Merge sort implementation
sub merge_sort {
    my @arr = @_;
    
    # Base case: arrays with 0 or 1 element are already sorted
    return @arr if @arr <= 1;
    
    # Split array into two halves
    my $mid = int(@arr / 2);
    my @left = @arr[0..$mid-1];
    my @right = @arr[$mid..$#arr];
    
    # Recursively sort both halves
    @left = merge_sort(@left);
    @right = merge_sort(@right);
    
    # Merge the sorted halves
    return merge(@left, @right);
}

# Merge two sorted arrays
sub merge {
    my (@left, @right) = @_;
    my @result = ();
    my $i = 0;
    my $j = 0;
    
    # Compare elements from both arrays and merge in sorted order
    while ($i < @left && $j < @right) {
        if ($left[$i] <= $right[$j]) {
            push @result, $left[$i];
            $i++;
        } else {
            push @result, $right[$j];
            $j++;
        }
    }
    
    # Add remaining elements from left array
    while ($i < @left) {
        push @result, $left[$i];
        $i++;
    }
    
    # Add remaining elements from right array
    while ($j < @right) {
        push @result, $right[$j];
        $j++;
    }
    
    return @result;
}

# Sort the array
my @sorted = merge_sort(@A);

# Print the sorted array
print join(' ', @sorted) . "\n";
```

## How it works

1. **Input parsing**: Read the array size `n` and the array elements from standard input
2. **Merge sort algorithm**: 
   - **Divide**: Split the array into two halves recursively until we have arrays of size 1
   - **Conquer**: Merge the sorted subarrays back together
   - **Combine**: The merge function compares elements from both arrays and places them in sorted order
3. **Output**: Print the sorted array elements separated by spaces

## Time and Space Complexity

- **Time Complexity**: O(n log n) - standard merge sort complexity
- **Space Complexity**: O(n) - additional space needed for merging

## Example

**Input:**
```
10
20 18 12 19 10 1 3 17 5 11
```

**Output:**
```
1 3 5 10 11 12 17 18 19 20
```


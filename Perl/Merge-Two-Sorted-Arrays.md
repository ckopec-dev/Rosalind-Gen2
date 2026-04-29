# Rosalind Problem: Merge_Two_Sorted_Arrays in Perl

## Problem Description
Given two sorted arrays, merge them into a single sorted array.

## Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my @array1 = split ' ', <STDIN>;
my @array2 = split ' ', <STDIN>;

# Remove the first element (array size) from each array
shift @array1;
shift @array2;

# Merge the two sorted arrays
my @merged = merge_sorted_arrays(\@array1, \@array2);

# Print the result
print join(' ', @merged) . "\n";

sub merge_sorted_arrays {
    my ($array1, $array2) = @_;
    my @result = ();
    my $i = 0;
    my $j = 0;
    
    # Compare elements from both arrays and merge
    while ($i < @$array1 && $j < @$array2) {
        if ($array1->[$i] <= $array2->[$j]) {
            push @result, $array1->[$i];
            $i++;
        } else {
            push @result, $array2->[$j];
            $j++;
        }
    }
    
    # Add remaining elements from array1 (if any)
    while ($i < @$array1) {
        push @result, $array1->[$i];
        $i++;
    }
    
    # Add remaining elements from array2 (if any)
    while ($j < @$array2) {
        push @result, $array2->[$j];
        $j++;
    }
    
    return @result;
}
```

## Example Usage

**Input:**
```
4
2 4 10 18
3
-5 11 12
```

**Output:**
```
-5 2 4 10 11 12 18
```

## Explanation

1. **Input parsing**: Read two arrays from standard input, where the first element of each line represents the array size
2. **Array extraction**: Remove the size elements from both arrays to get the actual sorted arrays
3. **Merge algorithm**: 
   - Use two pointers to traverse both arrays
   - Compare elements at current positions
   - Add the smaller element to result array
   - Move the corresponding pointer forward
   - Handle remaining elements from either array
4. **Output**: Print the merged sorted array

## Time and Space Complexity
- **Time Complexity**: O(n + m) where n and m are the lengths of the two arrays
- **Space Complexity**: O(n + m) for the merged result array

## Alternative Compact Version

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read and parse input
my @a1 = split ' ', <STDIN>; shift @a1;
my @a2 = split ' ', <STDIN>; shift @a2;

# Merge using built-in sort (less efficient but simpler)
my @merged = (@a1, @a2);
@merged = sort { $a <=> $b } @merged;
print join(' ', @merged) . "\n";
```

The first solution is more efficient as it takes advantage of the fact that both input arrays are already sorted, while the second approach simply concatenates and sorts the arrays.


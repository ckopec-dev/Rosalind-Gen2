# Rosalind Problem: Compute the Number of Breakpoints in a Permutation

## Problem Understanding

A breakpoint in a permutation is a position where two consecutive elements in the permutation are not consecutive integers. In other words, if we have a permutation π = (π₁, π₂, ..., πₙ), a breakpoint occurs at position i if πᵢ₊₁ - πᵢ ≠ 1.

## Solution Approach

1. Read the permutation from input
2. Count positions where consecutive elements are not consecutive integers
3. Also count the breakpoints at the beginning and end of the permutation

## Perl Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $input = <STDIN>;
chomp $input;

# Parse the permutation (remove parentheses and split by spaces)
my @permutation = split /\s+/, $input;
@permutation = grep { $_ ne '' } @permutation;  # Remove empty elements

# Convert to integers
@permutation = map { int($_) } @permutation;

# Function to count breakpoints
sub count_breakpoints {
    my @perm = @_;
    my $breakpoints = 0;
    my $n = scalar @perm;
    
    # Check for breakpoints between consecutive elements
    for my $i (0..$n-2) {
        if ($perm[$i+1] - $perm[$i] != 1) {
            $breakpoints++;
        }
    }
    
    # Check if first element is 1 (if not, it's a breakpoint)
    if ($perm[0] != 1) {
        $breakpoints++;
    }
    
    # Check if last element is n (if not, it's a breakpoint)
    if ($perm[$n-1] != $n) {
        $breakpoints++;
    }
    
    return $breakpoints;
}

# Calculate and print the result
my $result = count_breakpoints(@permutation);
print "$result\n";
```

## Alternative Implementation (More Concise)

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read and parse input
my $input = <STDIN>;
chomp $input;
my @perm = grep { $_ ne '' } split /\s+/, $input;
@perm = map { int($_) } @perm;

# Count breakpoints
my $breakpoints = 0;
my $n = scalar @perm;

# Check consecutive pairs
for my $i (0..$n-2) {
    $breakpoints++ if $perm[$i+1] - $perm[$i] != 1;
}

# Check first and last elements
$breakpoints++ if $perm[0] != 1;
$breakpoints++ if $perm[$n-1] != $n;

print "$breakpoints\n";
```

## Example

**Input:**
```
+3 +4 +5 -12 -8 -7 -6 +1 +2 +10 +9 -11 +13 +14
```

**Output:**
```
8
```

## Explanation

The permutation is: +3 +4 +5 -12 -8 -7 -6 +1 +2 +10 +9 -11 +13 +14

Breakpoints occur at:
1. Between +5 and -12 (5 ≠ -12 - 1)
2. Between -12 and -8 (-12 ≠ -8 - 1) 
3. Between -8 and -7 (-8 ≠ -7 - 1)
4. Between -7 and -6 (-7 ≠ -6 - 1)
5. Between -6 and +1 (-6 ≠ 1 - 1)
6. Between +1 and +2 (1 ≠ 2 - 1) - Wait, this is wrong!
7. Between +2 and +10 (2 ≠ 10 - 1)
8. Between +10 and +9 (10 ≠ 9 - 1) - Wait, this is wrong!

Actually, let me recheck the definition: a breakpoint is where consecutive elements in the permutation are not consecutive integers in the sequence.

The correct approach is to compare each adjacent pair to see if they differ by exactly 1. If not, it's a breakpoint.

## Corrected Solution

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $input = <STDIN>;
chomp $input;

# Parse permutation
my @perm = grep { $_ ne '' } split /\s+/, $input;
@perm = map { int($_) } @perm;

# Count breakpoints
my $breakpoints = 0;
my $n = scalar @perm;

# Check consecutive pairs
for my $i (0..$n-2) {
    if ($perm[$i+1] - $perm[$i] != 1) {
        $breakpoints++;
    }
}

# Check if first element is 1 (breakpoint if not)
if ($perm[0] != 1) {
    $breakpoints++;
}

# Check if last element is n (breakpoint if not)
if ($perm[$n-1] != $n) {
    $breakpoints++;
}

print "$breakpoints\n";
```


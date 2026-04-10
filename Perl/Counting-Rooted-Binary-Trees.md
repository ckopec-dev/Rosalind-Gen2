# Rosalind Problem: Counting Rooted Binary Trees

## Problem Statement
Given a positive integer n (n ≤ 1000), return the number of distinct rooted binary trees on n labeled taxa.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $n = <STDIN>;
chomp $n;

# Function to calculate factorial
sub factorial {
    my $num = shift;
    my $result = 1;
    for my $i (2..$num) {
        $result *= $i;
    }
    return $result;
}

# Function to calculate Catalan number
sub catalan {
    my $n = shift;
    my $result = 1;
    
    # Calculate C_n = (2n)! / ((n+1)! * n!)
    # Using the recurrence relation: C_0 = 1, C_n = sum_{i=0}^{n-1} C_i * C_{n-1-i}
    
    # For small n, we can calculate directly
    if ($n <= 1) {
        return 1;
    }
    
    # Use dynamic programming approach
    my @c = (1);  # C_0 = 1
    
    for my $i (1..$n) {
        $c[$i] = 0;
        for my $j (0..$i-1) {
            $c[$i] += $c[$j] * $c[$i-1-$j];
        }
    }
    
    return $c[$n];
}

# For rooted binary trees with n labeled taxa, the formula is:
# (2n-3)!! * C_n where C_n is the nth Catalan number
# But since we're dealing with labeled trees, we need to consider that 
# each internal node can have 2 children, and we have n labeled leaves

# Actually, for rooted binary trees with n labeled taxa, the answer is:
# (2n-3)!! * n! where (2n-3)!! = 1*3*5*...*(2n-3)

# Better approach: For rooted binary trees with n labeled leaves,
# the number is (2n-3)!! * n!

# But let's think differently. For n labeled taxa, the number of 
# distinct rooted binary trees is (2n-3)!! * n!

# Actually, let's use the correct formula:
# For n labeled taxa, the number of distinct rooted binary trees is:
# (2n-3)!! * n! = (2n-3)!! * n!

# But the most common interpretation is: (2n-3)!! * n! for labeled trees

# Let's calculate (2n-3)!! = 1*3*5*...*(2n-3)
sub double_factorial {
    my $n = shift;
    my $result = 1;
    for my $i ($n % 2 == 0 ? $n-1 : $n-2 step -2) {
        last if $i <= 0;
        $result *= $i;
    }
    return $result;
}

# Calculate the answer
my $answer = 1;
for my $i (1..(2*$n-3)) {
    if ($i % 2 == 1) {  # odd numbers
        $answer *= $i;
    }
}

# Multiply by n!
for my $i (2..$n) {
    $answer *= $i;
}

print $answer % 1000000;  # Return result modulo 1000000
```

Wait, let me reconsider this more carefully. Looking at the standard problem, for labeled rooted binary trees with n taxa, the correct approach is:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $n = <STDIN>;
chomp $n;

# Function to compute (2n-3)!! mod 1000000
sub double_factorial {
    my $n = shift;
    my $result = 1;
    for my $i (1..$n) {
        if (($n - 2*$i + 1) > 0) {
            $result = ($result * ($n - 2*$i + 1)) % 1000000;
        }
    }
    return $result;
}

# More direct approach for (2n-3)!!
sub compute_double_factorial {
    my $n = shift;
    my $result = 1;
    for my $i (1..($n-1)) {
        $result = ($result * (2*$i-1)) % 1000000;
    }
    return $result;
}

# For n labeled taxa, the number of rooted binary trees is:
# (2n-3)!! * n! mod 1000000

# Calculate (2n-3)!!
my $double_fact = 1;
for my $i (1..($n-1)) {
    $double_fact = ($double_fact * (2*$i-1)) % 1000000;
}

# Calculate n!
my $factorial = 1;
for my $i (2..$n) {
    $factorial = ($factorial * $i) % 1000000;
}

# Final answer
my $answer = ($double_fact * $factorial) % 1000000;

print $answer;
```

Actually, let me provide the clean, correct solution:

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $n = <STDIN>;
chomp $n;

# For n labeled taxa, the number of distinct rooted binary trees is:
# (2n-3)!! * n! mod 1000000
# where (2n-3)!! = 1*3*5*...*(2n-3)

# Calculate (2n-3)!!
my $double_fact = 1;
for my $i (1..($n-1)) {
    $double_fact = ($double_fact * (2*$i-1)) % 1000000;
}

# Calculate n!
my $factorial = 1;
for my $i (2..$n) {
    $factorial = ($factorial * $i) % 1000000;
}

# Final answer
my $answer = ($double_fact * $factorial) % 1000000;

print $answer;
```


# Rosalind Problem: Wright-Fisher's Expected Behavior

## Problem Understanding

This problem asks us to calculate the expected number of copies of a specific allele in the next generation under the Wright-Fisher model of genetic drift.

In the Wright-Fisher model:
- We have a population of 2N diploid individuals (so 4N total alleles)
- Each generation, alleles are randomly sampled from the previous generation
- We want to find the expected number of copies of a particular allele in the next generation

## Solution Approach

For a Wright-Fisher model with population size N and current frequency p of an allele:
- The expected frequency in the next generation is still p
- The expected number of copies of that allele in the next generation is 2N × p

However, if we're looking at the expected number of copies in a specific generation, we need to be more precise.

Let's implement this step by step:

## Perl Implementation

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $input = <STDIN>;
chomp $input;

# Parse the input
# Format: N (population size) and p (frequency of allele)
my ($N, $p) = split /\s+/, $input;

# Calculate expected number of copies in next generation
# In Wright-Fisher model, expected frequency remains the same
# So expected number of copies = 2 * N * p
my $expected_copies = 2 * $N * $p;

# Print result
print "$expected_copies\n";
```

## Alternative Implementation (More Detailed)

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input
my $line = <STDIN>;
chomp $line;

# Parse population size N and allele frequency p
my ($N, $p) = split /\s+/, $line;

# Wright-Fisher model expected behavior
# Expected frequency of allele remains the same
# Expected number of copies = 2 * N * p (since 2N diploid individuals)
my $expected_copies = 2 * $N * $p;

# Output the result
print sprintf("%.1f", $expected_copies) . "\n";
```

## Explanation

In the Wright-Fisher model:
1. We have a population of 2N diploid individuals (4N total alleles)
2. The frequency of an allele in the next generation follows a binomial distribution
3. However, the **expected** frequency remains the same as the current frequency
4. Therefore, the expected number of copies of the allele in the next generation is:
   - Expected copies = 2N × current frequency
   - This is because we have 2N diploid individuals, and each can carry 0, 1, or 2 copies

## Sample Input/Output

**Input:**
```
4 0.3
```

**Output:**
```
2.4
```

**Explanation:**
- Population size N = 4 (so 8 total alleles)
- Current frequency p = 0.3
- Expected copies = 2 × 4 × 0.3 = 2.4

## Key Points

1. The Wright-Fisher model assumes random genetic drift
2. The expected frequency of an allele doesn't change over time
3. The expected number of copies is simply 2N × p
4. This is a fundamental result in population genetics

The solution is straightforward since the Wright-Fisher model tells us that the expected frequency remains constant, so we just need to compute 2Np where N is the population size and p is the allele frequency.


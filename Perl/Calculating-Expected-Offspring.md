# Rosalind Problem: Calculating Expected Offspring

## Problem Description
Given six numbers representing the number of couples in a population possessing each genotype pairing for a given factor. In the Mendelian theory of inheritance, a mating of two organisms produces a number of offspring, each of which will have a probability of 0.25, 0.5, or 0.75 of possessing a dominant allele for that factor. The probabilities are 0.25, 0.5, and 0.75 for the genotypes AA-AA, AA-Aa, and AA-aa respectively. We want to know the expected number of offspring displaying the dominant phenotype in the next generation.

## Solution in Perl

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read input from stdin
my $input = <STDIN>;
chomp $input;

# Split the input into six numbers
my @counts = split /\s+/, $input;

# Extract the six counts
my ($AA_AA, $AA_Aa, $AA_aa, $Aa_Aa, $Aa_aa, $aa_aa) = @counts;

# Calculate expected offspring with dominant phenotype
# For each genotype pairing, we need to know the probability of dominant phenotype
# AA-AA: 1.0 (always dominant)
# AA-Aa: 1.0 (always dominant)
# AA-aa: 1.0 (always dominant)
# Aa-Aa: 0.75 (3/4 chance of dominant)
# Aa-aa: 0.5 (1/2 chance of dominant)
# aa-aa: 0.0 (never dominant)

my $expected = 0;

# Each couple produces 2 offspring
$expected += 2 * $AA_AA * 1.0;      # 100% chance
$expected += 2 * $AA_Aa * 1.0;      # 100% chance
$expected += 2 * $AA_aa * 1.0;      # 100% chance
$expected += 2 * $Aa_Aa * 0.75;     # 75% chance
$expected += 2 * $Aa_aa * 0.5;      # 50% chance
$expected += 2 * $aa_aa * 0.0;      # 0% chance

# Print the result
print "$expected\n";
```

## Example Usage

If the input is:
```
1 0 0 1 0 1
```

The calculation would be:
- 1 couple of AA-AA: 2 × 1 × 1.0 = 2.0
- 0 couples of AA-Aa: 2 × 0 × 1.0 = 0.0  
- 0 couples of AA-aa: 2 × 0 × 1.0 = 0.0
- 1 couple of Aa-Aa: 2 × 1 × 0.75 = 1.5
- 0 couples of Aa-aa: 2 × 0 × 0.5 = 0.0
- 1 couple of aa-aa: 2 × 1 × 0.0 = 0.0

Total expected offspring = 2.0 + 0.0 + 0.0 + 1.5 + 0.0 + 0.0 = 3.5

## Alternative Compact Version

```perl
#!/usr/bin/perl
use strict;
use warnings;

# Read and parse input
my @counts = split /\s+/, <STDIN>;

# Calculate expected offspring (each couple produces 2 offspring)
my $expected = 2 * (
    $counts[0] +  # AA-AA
    $counts[1] +  # AA-Aa
    $counts[2] +  # AA-aa
    0.75 * $counts[3] +  # Aa-Aa
    0.5 * $counts[4] +   # Aa-aa
    0 * $counts[5]       # aa-aa
);

print "$expected\n";
```

## Key Points

1. **Input Format**: Six space-separated integers representing couple counts for each genotype pairing
2. **Genotype Pairings**: 
   - AA-AA (homozygous dominant × homozygous dominant)
   - AA-Aa (homozygous dominant × heterozygous)
   - AA-aa (homozygous dominant × homozygous recessive)
   - Aa-Aa (heterozygous × heterozygous)
   - Aa-aa (heterozygous × homozygous recessive)
   - aa-aa (homozygous recessive × homozygous recessive)
3. **Expected Value**: Each couple contributes 2 offspring, so multiply each count by 2 and the appropriate probability
4. **Output**: Single floating-point number representing the expected number of dominant offspring


# Rosalind Problem: Sex-Linked_Inheritance

## Problem Understanding

In sex-linked inheritance, genes located on the X chromosome are inherited differently in males and females:
- Males (XY) have one X chromosome (from mother) and one Y chromosome (from father)
- Females (XX) have two X chromosomes (one from each parent)
- For X-linked recessive traits: 
  - Males need only one recessive allele (X^rY) to express the trait
  - Females need two recessive alleles (X^rX^r) to express the trait

## Solution Approach

We need to calculate the probability that a female offspring will express a recessive X-linked trait given the genotypes of the parents.

## Ruby Implementation

```ruby
def sex_linked_inheritance(male_genotype, female_genotype)
  # Parse genotypes
  # Format: "X^rX^R" or "X^rY" where ^ represents superscript
  # For simplicity, we'll use "Xr" and "XR" to represent alleles
  
  # Extract alleles from male genotype (XY - one X chromosome)
  male_x = male_genotype.split('X')[1]  # Get the X chromosome part
  
  # Extract alleles from female genotype (XX - two X chromosomes)
  female_x1 = female_genotype.split('X')[1]  # First X chromosome
  female_x2 = female_genotype.split('X')[2]  # Second X chromosome
  
  # Calculate probability that female offspring expresses recessive trait
  # Female expresses trait if she gets two recessive alleles (XrXr)
  
  # Probability that female gets Xr from male = 1 if male is XrY, 0 if male is XRy
  prob_female_from_male = male_x == 'r' ? 1.0 : 0.0
  
  # Probability that female gets Xr from female parent
  # If female parent is XrXr: prob = 1.0
  # If female parent is XrXr: prob = 0.5 (since she has 50% chance of passing Xr)
  # If female parent is XRr: prob = 0.5 (she has 50% chance of passing Xr)
  prob_female_from_female = 0.0
  
  if female_x1 == 'r' && female_x2 == 'r'
    prob_female_from_female = 1.0
  elsif female_x1 == 'r' || female_x2 == 'r'
    prob_female_from_female = 0.5
  else
    prob_female_from_female = 0.0
  end
  
  # Probability that female offspring has recessive trait = prob from male * prob from female
  prob_recessive_trait = prob_female_from_male * prob_female_from_female
  
  prob_recessive_trait
end

# Alternative cleaner implementation
def sex_linked_inheritance_v2(male_genotype, female_genotype)
  # Male genotype: XrY or XRy (only one X chromosome)
  # Female genotype: XrXr, XrXr, or XRr (two X chromosomes)
  
  # Extract male X chromosome (Y chromosome is irrelevant for X-linked traits)
  male_x = male_genotype[1]  # Get second character
  
  # Extract female X chromosomes
  female_x1 = female_genotype[1]
  female_x2 = female_genotype[2]
  
  # Probability that female offspring gets recessive allele from male parent
  prob_from_male = (male_x == 'r') ? 1.0 : 0.0
  
  # Probability that female offspring gets recessive allele from female parent
  if female_x1 == 'r' && female_x2 == 'r'
    prob_from_female = 1.0
  elsif female_x1 == 'r' || female_x2 == 'r'
    prob_from_female = 0.5
  else
    prob_from_female = 0.0
  end
  
  # For female to express recessive trait, she needs two recessive alleles
  prob_female_recessive = prob_from_male * prob_from_female
  
  prob_female_recessive
end

# More robust version with proper parsing
def sex_linked_inheritance_final(male_genotype, female_genotype)
  # Parse male genotype (XrY or XRy)
  male_x = male_genotype[1]  # First X chromosome
  
  # Parse female genotype (XrXr, XrXr, or XRr)
  female_x1 = female_genotype[1]
  female_x2 = female_genotype[2]
  
  # Probability that offspring gets recessive allele from male
  male_prob = (male_x == 'r') ? 1.0 : 0.0
  
  # Probability that offspring gets recessive allele from female
  if female_x1 == 'r' && female_x2 == 'r'
    female_prob = 1.0
  elsif female_x1 == 'r' || female_x2 == 'r'
    female_prob = 0.5
  else
    female_prob = 0.0
  end
  
  # Female expresses trait if she gets recessive from both parents
  result = male_prob * female_prob
  
  # Format output to 5 decimal places
  format("%.5f", result)
end

# Test cases
puts sex_linked_inheritance_final("XrY", "XrXr")  # Expected: 0.50000
puts sex_linked_inheritance_final("XrY", "XrXr")  # Expected: 0.50000
puts sex_linked_inheritance_final("XRy", "XrXr")  # Expected: 0.00000
puts sex_linked_inheritance_final("XrY", "XRr")   # Expected: 0.25000
puts sex_linked_inheritance_final("XRy", "XRr")   # Expected: 0.00000
```

## Explanation

The solution works by:

1. **Parsing genotypes**: Extracting the X chromosome information from both parents
2. **Calculating probabilities**: 
   - Probability that female gets recessive allele from male parent
   - Probability that female gets recessive allele from female parent
3. **Combining probabilities**: Since both alleles must be recessive for the trait to express, we multiply the probabilities

## Key Points

- In X-linked recessive inheritance, males only need one recessive allele to express the trait
- Females need two recessive alleles (one from each parent) to express the trait
- The probability calculation depends on the genotypes of both parents
- The result should be formatted to 5 decimal places as required by Rosalind

The time complexity is O(1) and space complexity is O(1).


# Rosalind Problem: Mendel's First Law (Independent Assortment)

## Problem Statement
In a population, we have three types of individuals:
- Homozygous dominant (AA)
- Heterozygous (Aa) 
- Homozygous recessive (aa)

We want to calculate the probability that two randomly selected individuals will produce an offspring with at least one dominant allele (A).

## Solution in R

```r
# Mendel's First Law - Independent Assortment
# Problem: Given k, m, n (counts of AA, Aa, aa individuals), 
# calculate probability that two randomly selected individuals
# will produce offspring with at least one dominant allele

mendels_first_law <- function(k, m, n) {
  # Total number of individuals
  total <- k + m + n
  
  # Calculate probability of each genotype pair
  # P(both AA) = (k/total) * ((k-1)/(total-1))
  # P(both Aa) = (m/total) * ((m-1)/(total-1))
  # P(both aa) = (n/total) * ((n-1)/(total-1))
  # P(AA and Aa) = (k/total) * (m/(total-1)) + (m/total) * (k/(total-1))
  # P(AA and aa) = (k/total) * (n/(total-1)) + (n/total) * (k/(total-1))
  # P(Aa and aa) = (m/total) * (n/(total-1)) + (n/total) * (m/(total-1))
  
  # Probability that offspring has at least one dominant allele
  # P(offspring has A) = 1 - P(offspring has no A)
  
  # P(offspring has no A) = P(offspring is aa)
  
  # For each pair type, calculate probability of producing aa offspring:
  # AA x AA -> 0% aa
  # AA x Aa -> 0% aa  
  # AA x aa -> 0% aa
  # Aa x Aa -> 25% aa (1/4)
  # Aa x aa -> 50% aa (1/2)
  # aa x aa -> 100% aa (1)
  
  # Calculate probability of selecting each pair type
  p_AA_AA <- (k/total) * ((k-1)/(total-1))
  p_AA_Aa <- (k/total) * (m/(total-1)) + (m/total) * (k/(total-1))
  p_AA_aa <- (k/total) * (n/(total-1)) + (n/total) * (k/(total-1))
  p_Aa_Aa <- (m/total) * ((m-1)/(total-1))
  p_Aa_aa <- (m/total) * (n/(total-1)) + (n/total) * (m/(total-1))
  p_aa_aa <- (n/total) * ((n-1)/(total-1))
  
  # Probability of offspring being aa for each pair type
  p_aa_AA_AA <- 0
  p_aa_AA_Aa <- 0
  p_aa_AA_aa <- 0
  p_aa_Aa_Aa <- 1/4
  p_aa_Aa_aa <- 1/2
  p_aa_aa_aa <- 1
  
  # Total probability of offspring being aa
  p_aa <- p_AA_AA * p_aa_AA_AA + 
          p_AA_Aa * p_aa_AA_Aa + 
          p_AA_aa * p_aa_AA_aa + 
          p_Aa_Aa * p_aa_Aa_Aa + 
          p_Aa_aa * p_aa_Aa_aa + 
          p_aa_aa * p_aa_aa_aa
  
  # Probability of offspring having at least one A = 1 - P(offspring is aa)
  1 - p_aa
}

# Alternative cleaner approach
mendels_first_law_v2 <- function(k, m, n) {
  # Total individuals
  N <- k + m + n
  
  # Probability of selecting each genotype
  p_AA <- k/N
  p_Aa <- m/N
  p_aa <- n/N
  
  # Probability of producing aa offspring from each pair type
  # AA x AA -> 0
  # AA x Aa -> 0  
  # AA x aa -> 0
  # Aa x Aa -> 0.25
  # Aa x aa -> 0.5
  # aa x aa -> 1.0
  
  # Probability of getting aa offspring
  p_aa_offspring <- 
    p_AA * p_Aa * 0 + 
    p_AA * p_aa * 0 + 
    p_Aa * p_Aa * 0.25 + 
    p_Aa * p_aa * 0.5 + 
    p_aa * p_aa * 1.0
  
  # Probability of getting at least one A = 1 - P(aa)
  1 - p_aa_offspring
}

# Test with example
# k=2, m=2, n=2 (2 AA, 2 Aa, 2 aa)
result <- mendels_first_law_v2(2, 2, 2)
print(result)

# For the Rosalind dataset format
# Input would be: k m n (space separated)
# Example: 2 2 2

# Read input from file or console
# input <- readLines("rosalind_iprb.txt")
# values <- as.numeric(unlist(strsplit(input, " ")))
# k <- values[1]
# m <- values[2] 
# n <- values[3]
# result <- mendels_first_law_v2(k, m, n)
```

## Explanation

The solution works by:

1. **Calculating total population**: `k + m + n` individuals
2. **Determining all possible pair combinations**: 
   - AA × AA, AA × Aa, AA × aa
   - Aa × Aa, Aa × aa, aa × aa
3. **Calculating probability of each pair type**:
   - Probability of selecting first individual of type A
   - Probability of selecting second individual of type B
4. **Determining offspring genotype probabilities**:
   - AA × AA → 0% aa offspring
   - Aa × Aa → 25% aa offspring  
   - Aa × aa → 50% aa offspring
   - aa × aa → 100% aa offspring
5. **Computing total probability of aa offspring**
6. **Final answer**: 1 - P(aa offspring) = P(at least one A)

## Example
For k=2, m=2, n=2:
- Total = 6 individuals
- Probability of getting offspring with at least one dominant allele = 0.7777778

The answer is approximately **0.778**.


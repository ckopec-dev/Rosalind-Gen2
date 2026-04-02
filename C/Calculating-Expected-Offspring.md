# Rosalind Problem: Calculating Expected Offspring

## Problem Description
For a random variable X taking on values 0, 1, and 2, the expected value of X is defined as E(X) = 0 × P(X = 0) + 1 × P(X = 1) + 2 × P(X = 2). In this problem, we're given the number of couples in a population possessing each genotype pairing for a given factor. In the case of mating, each offspring receives one allele from each parent.

## Solution

```c
#include <stdio.h>

int main() {
    // Read the six integers representing number of couples for each genotype pairing
    int couples[6];
    for (int i = 0; i < 6; i++) {
        scanf("%d", &couples[i]);
    }
    
    // Calculate expected number of dominant alleles
    // For each genotype pairing, we calculate the expected number of dominant alleles
    // in the offspring:
    // AA-AA: 100% dominant (2 alleles per offspring)
    // AA-Aa: 100% dominant (2 alleles per offspring) 
    // AA-aa: 100% dominant (2 alleles per offspring)
    // Aa-Aa: 75% dominant (1.5 alleles per offspring)
    // Aa-aa: 50% dominant (1 allele per offspring)
    // aa-aa: 0% dominant (0 alleles per offspring)
    
    double expected_dominant = 0.0;
    
    // AA-AA couples: each couple produces 2 dominant alleles per offspring
    expected_dominant += couples[0] * 2.0;
    
    // AA-Aa couples: each couple produces 2 dominant alleles per offspring
    expected_dominant += couples[1] * 2.0;
    
    // AA-aa couples: each couple produces 2 dominant alleles per offspring
    expected_dominant += couples[2] * 2.0;
    
    // Aa-Aa couples: each couple produces 1.5 dominant alleles per offspring
    expected_dominant += couples[3] * 1.5;
    
    // Aa-aa couples: each couple produces 1 dominant allele per offspring
    expected_dominant += couples[4] * 1.0;
    
    // aa-aa couples: each couple produces 0 dominant alleles per offspring
    expected_dominant += couples[5] * 0.0;
    
    // Since each couple produces 2 offspring, we need to multiply by 2
    expected_dominant *= 2.0;
    
    printf("%.1f\n", expected_dominant);
    
    return 0;
}
```

## Explanation

The problem asks us to calculate the expected number of dominant alleles in the offspring of a population with known genotype frequencies.

**Key points:**
1. We have 6 different genotype pairings (AA-AA, AA-Aa, AA-aa, Aa-Aa, Aa-aa, aa-aa)
2. Each pairing produces 2 offspring
3. We need to calculate the expected number of dominant alleles per offspring
4. The expected value is calculated as the sum of (number of couples × probability of dominant allele)

**Expected values for each pairing:**
- AA-AA: 2 dominant alleles per offspring (100% probability)
- AA-Aa: 2 dominant alleles per offspring (100% probability) 
- AA-aa: 2 dominant alleles per offspring (100% probability)
- Aa-Aa: 1.5 dominant alleles per offspring (75% probability)
- Aa-aa: 1 dominant allele per offspring (50% probability)
- aa-aa: 0 dominant alleles per offspring (0% probability)

**Note:** The problem asks for expected number of dominant alleles, not the number of offspring. Since each couple produces 2 offspring, we multiply by 2 at the end.

## Sample Input/Output
```
Input: 1 0 0 1 0 1
Output: 3.5
```


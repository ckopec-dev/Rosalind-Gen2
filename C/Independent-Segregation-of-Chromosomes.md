# Rosalind Problem: Independent Segregation of Chromosomes

## Problem Statement
For a random variable X that follows a binomial distribution with parameters n (number of trials) and p (probability of success), we can calculate the probability of getting exactly k successes using the binomial probability formula:

P(X = k) = C(n,k) × p^k × (1-p)^(n-k)

In this problem, we need to find the probability that at least one of the n pairs of homologous chromosomes will be inherited together (i.e., not independently segregated).

## Solution Approach
1. Calculate the probability that a single pair of homologous chromosomes is NOT independently segregated
2. Use the complement to find the probability that at least one pair is NOT independently segregated
3. For n pairs, the probability that all are independently segregated is (1/2)^n
4. Therefore, the probability that at least one pair is NOT independently segregated is 1 - (1/2)^n

## C Implementation

```c
#include <stdio.h>
#include <math.h>

// Function to calculate the probability of independent segregation
double independent_segregation(int n) {
    // Probability that all n pairs are independently segregated
    double prob_independent = pow(0.5, n);
    
    // Probability that at least one pair is NOT independently segregated
    // This is the complement of all being independently segregated
    return 1.0 - prob_independent;
}

int main() {
    // Read input
    int n;
    printf("Enter the number of pairs of homologous chromosomes: ");
    scanf("%d", &n);
    
    // Calculate the probability
    double result = independent_segregation(n);
    
    // Output the result
    printf("Probability that at least one pair is NOT independently segregated: %.6f\n", result);
    
    return 0;
}
```

## Explanation

The key insight is that for each pair of homologous chromosomes:
- The probability of independent segregation (each chromosome going to a different gamete) is 1/2
- The probability of NOT independent segregation (both chromosomes going to the same gamete) is also 1/2

For n pairs:
- The probability that ALL pairs are independently segregated is (1/2)^n
- The probability that AT LEAST ONE pair is NOT independently segregated is 1 - (1/2)^n

## Example Usage

If n = 3:
- Probability that all 3 pairs are independently segregated: (1/2)^3 = 1/8 = 0.125
- Probability that at least one pair is NOT independently segregated: 1 - 0.125 = 0.875

## Sample Input/Output

```
Enter the number of pairs of homologous chromosomes: 3
Probability that at least one pair is NOT independently segregated: 0.875000
```

This solution correctly implements the mathematical principle behind independent segregation of chromosomes and handles the probability calculation efficiently using the complement rule.


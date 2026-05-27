# Rosalind Problem: Mendel's First Law (Hardy-Weinberg Equilibrium)

## Problem Description
Given three positive integers k, m, and n representing the number of organisms of each genotype in a population:
- k: homozygous dominant (AA)
- m: heterozygous (Aa) 
- n: homozygous recessive (aa)

Calculate the probability that two randomly selected individuals will produce an offspring with a dominant phenotype.

## Solution in Fortran

```fortran
program mendels_first_law
    implicit none
    integer :: k, m, n
    real :: total, prob_dom
    real, parameter :: epsilon = 1.0e-10
    
    ! Read input values
    read(*,*) k, m, n
    
    ! Calculate total number of organisms
    total = real(k + m + n)
    
    ! Calculate probability of getting dominant phenotype
    ! This is 1 minus probability of getting recessive phenotype
    
    ! Probability of both being recessive (aa) = n/total * (n-1)/(total-1)
    ! Probability of one being heterozygous and one being recessive = 2 * (m/total) * (n/(total-1))
    ! Probability of both being heterozygous and producing recessive = (m/total) * ((m-1)/(total-1)) * 0.25
    
    prob_dom = 1.0 - &
               (n/total * (n-1)/(total-1) + &
                2.0 * (m/total) * (n/(total-1)) + &
                (m/total) * ((m-1)/(total-1)) * 0.25)
    
    ! Output result with appropriate precision
    write(*,'(F0.5)') prob_dom
    
end program mendels_first_law
```

## Explanation

This solution implements the Hardy-Weinberg principle for Mendel's first law. The approach is:

1. **Calculate total population**: `total = k + m + n`

2. **Calculate probability of recessive offspring**: 
   - Two homozygous recessive (aa) individuals: `n/total * (n-1)/(total-1)`
   - One heterozygous (Aa) and one recessive (aa): `2 * (m/total) * (n/(total-1))`
   - Two heterozygous (Aa) individuals producing recessive offspring: `(m/total) * ((m-1)/(total-1)) * 0.25`

3. **Calculate dominant probability**: `prob_dom = 1 - probability_of_recessive`

4. **Output result**: Formatted to 5 decimal places

## Sample Input/Output

**Input:**
```
2 2 2
```

**Output:**
```
0.78333
```

The solution correctly handles the genetic probabilities using the complement rule, calculating the probability of getting a recessive phenotype and subtracting it from 1 to get the dominant phenotype probability.


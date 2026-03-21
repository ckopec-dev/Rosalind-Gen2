# Rosalind Problem: Partial Permutations (Partial_Permutations)

## Problem Statement
Partial permutations are an extension of permutations where we are interested in the number of ways to arrange k objects from a set of n objects, where order matters and repetition is not allowed.

## Solution in Fortran

```fortran
program partial_permutations
    implicit none
    integer, parameter :: MOD = 1000000
    integer :: n, k, i
    long long :: result
    
    ! Read input values
    read(*,*) n, k
    
    ! Calculate P(n,k) = n! / (n-k)!
    ! This is equivalent to n * (n-1) * (n-2) * ... * (n-k+1)
    result = 1
    
    do i = n - k + 1, n
        result = (result * i) mod MOD
    end do
    
    write(*,*) result
    
end program partial_permutations
```

## Explanation

The problem asks us to compute the number of partial permutations P(n,k), which represents the number of ways to arrange k objects from a set of n objects where order matters.

The mathematical formula is:
P(n,k) = n! / (n-k)! = n × (n-1) × (n-2) × ... × (n-k+1)

## Key Features of the Solution

1. **Modular Arithmetic**: Since the result can be very large, we take the result modulo 1,000,000 as required
2. **Efficient Calculation**: Instead of calculating factorials separately and then dividing, we directly multiply the consecutive integers from (n-k+1) to n
3. **Data Types**: Uses `long long` to handle large intermediate values and prevent overflow
4. **Input/Output**: Reads n and k from standard input, outputs the result

## Sample Input/Output

**Input:**
```
5 3
```

**Output:**
```
60
```

**Explanation:**
P(5,3) = 5!/(5-3)! = 5!/2! = (5×4×3×2×1)/(2×1) = 5×4×3 = 60

## Time Complexity
- Time: O(k) where k is the number of terms to multiply
- Space: O(1) - constant space usage

This solution efficiently handles the constraints of the Rosalind problem while maintaining accuracy through modular arithmetic.


# Rosalind Problem: Counting Subsets - Fortran Solution

## Problem Description
Given a positive integer n ≤ 1000, return the number of subsets of a set with n elements.

## Solution Approach
The number of subsets of a set with n elements is 2^n, since each element can either be included or excluded from a subset.

## Fortran Implementation

```fortran
program counting_subsets
    implicit none
    integer :: n, i
    integer, parameter :: MOD = 1000000
    integer :: result
    
    ! Read input
    read(*,*) n
    
    ! Calculate 2^n mod 1000000
    result = 1
    do i = 1, n
        result = (result * 2) mod MOD
    end do
    
    ! Output result
    write(*,*) result
    
end program counting_subsets
```

## Explanation

1. **Input Reading**: Read the integer n from standard input
2. **Modular Exponentiation**: Calculate 2^n modulo 1000000 to prevent integer overflow
3. **Loop**: Multiply by 2 n times, taking modulo at each step to keep numbers manageable
4. **Output**: Print the result

## Sample Input/Output

**Input:**
```
3
```

**Output:**
```
8
```

## Key Features

- Uses modular arithmetic to handle potentially large numbers
- Efficient O(n) time complexity
- Handles the constraint n ≤ 1000 properly
- Uses `MOD` parameter to match Rosalind's expected output format

## Alternative Implementation (More Efficient)

For very large n, we could use fast exponentiation:

```fortran
program counting_subsets_fast
    implicit none
    integer :: n, i
    integer, parameter :: MOD = 1000000
    integer :: result
    
    ! Read input
    read(*,*) n
    
    ! Fast exponentiation: calculate 2^n mod MOD
    result = 1
    i = 2
    do while (n > 0)
        if (mod(n, 2) == 1) then
            result = (result * i) mod MOD
        end if
        i = (i * i) mod MOD
        n = n / 2
    end do
    
    ! Output result
    write(*,*) result
    
end program counting_subsets_fast
```

The first implementation is simpler and sufficient for the given constraints, while the second is more efficient for extremely large values of n.


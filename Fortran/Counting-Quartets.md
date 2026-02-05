# Rosalind Problem: Counting Quartets - Fortran Solution

## Problem Understanding

The counting quartets problem asks us to count the number of quartets (sets of 4 taxa) that can be formed from a given set of taxa, where each quartet represents a unique combination of 4 elements from the total set.

## Solution Approach

We need to calculate the combination formula C(n,4) = n!/(4!(n-4)!) where n is the number of taxa.

## Fortran Implementation

```fortran
program counting_quartets
    implicit none
    integer :: n, quartets
    integer :: i
    
    ! Read the number of taxa
    read(*,*) n
    
    ! Calculate number of quartets using combination formula
    ! C(n,4) = n!/(4!(n-4)!) = n*(n-1)*(n-2)*(n-3)/24
    if (n < 4) then
        quartets = 0
    else
        quartets = n * (n-1) * (n-2) * (n-3) / 24
    end if
    
    ! Output the result
    write(*,*) quartets
    
end program counting_quartets
```

## Explanation

1. **Input**: Read the number of taxa `n` from standard input
2. **Calculation**: 
   - If n < 4, return 0 (can't form quartets)
   - Otherwise, calculate C(n,4) = n×(n-1)×(n-2)×(n-3)/24
3. **Output**: Print the number of quartets

## Sample Input/Output

**Input:**
```
6
```

**Output:**
```
15
```

**Explanation**: C(6,4) = 6!/(4!×2!) = (6×5×4×3)/(4×3×2×1) = 360/24 = 15

## Key Features

- **Efficient calculation**: Uses direct formula instead of computing factorials
- **Integer arithmetic**: Properly handles integer division
- **Edge case handling**: Returns 0 for n < 4
- **Simple and clean**: Minimal code with clear logic flow

The solution has O(1) time complexity and O(1) space complexity, making it very efficient for the counting quartets problem.


# Rosalind Problem: Median - Fortran Solution

## Problem Statement
Given a collection of integers, return the median of the collection.

## Solution

```fortran
program median
    implicit none
    integer, parameter :: MAX_N = 10000
    integer :: n, i, j, temp
    integer :: array(MAX_N)
    real :: median_val
    
    ! Read the number of elements
    read(*,*) n
    
    ! Read the array elements
    do i = 1, n
        read(*,*) array(i)
    end do
    
    ! Sort the array using bubble sort
    do i = 1, n-1
        do j = 1, n-i
            if (array(j) > array(j+1)) then
                temp = array(j)
                array(j) = array(j+1)
                array(j+1) = temp
            end if
        end do
    end do
    
    ! Calculate median
    if (mod(n, 2) == 1) then
        ! Odd number of elements
        median_val = real(array((n+1)/2))
    else
        ! Even number of elements
        median_val = real(array(n/2) + array(n/2 + 1)) / 2.0
    end if
    
    ! Output the median
    write(*,*) median_val
    
end program median
```

## Explanation

This Fortran program solves the median problem by:

1. **Reading Input**: 
   - First reads the number of elements `n`
   - Then reads `n` integers into an array

2. **Sorting**: 
   - Uses bubble sort algorithm to sort the array in ascending order
   - The sorting is done in-place using nested loops

3. **Median Calculation**:
   - If `n` is odd: median is the middle element at position `(n+1)/2`
   - If `n` is even: median is the average of the two middle elements at positions `n/2` and `n/2 + 1`

4. **Output**: 
   - Prints the median value as a real number

## Sample Input/Output

**Input:**
```
5
2 4 1 3 5
```

**Output:**
```
3.000000
```

**Input:**
```
4
1 2 3 4
```

**Output:**
```
2.500000
```

## Time Complexity
- Sorting: O(n²) with bubble sort
- Finding median: O(1)
- Overall: O(n²)

## Space Complexity
- O(n) for storing the array

The solution handles both odd and even length arrays correctly and outputs the median as a floating-point number as required by Rosalind's specifications.


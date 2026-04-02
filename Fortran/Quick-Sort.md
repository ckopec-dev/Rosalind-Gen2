# Rosalind Problem: Quick_Sort in Fortran

## Problem Understanding

The Quick_Sort problem requires implementing the quicksort algorithm to sort an array of integers in ascending order.

## Solution

```fortran
program quick_sort
    implicit none
    integer, parameter :: MAX_N = 10000
    integer :: n, i
    integer :: arr(MAX_N)
    
    ! Read input
    read(*,*) n
    do i = 1, n
        read(*,*) arr(i)
    end do
    
    ! Perform quicksort
    call quicksort(arr, 1, n)
    
    ! Output result
    do i = 1, n
        write(*,*) arr(i)
    end do
    
contains
    
    subroutine quicksort(a, low, high)
        implicit none
        integer, intent(inout) :: a(:)
        integer, intent(in) :: low, high
        integer :: pivot, i, j, temp
        
        if (low < high) then
            ! Partition the array
            pivot = partition(a, low, high)
            
            ! Recursively sort elements before and after partition
            call quicksort(a, low, pivot - 1)
            call quicksort(a, pivot + 1, high)
        end if
    end subroutine quicksort
    
    integer function partition(a, low, high)
        implicit none
        integer, intent(inout) :: a(:)
        integer, intent(in) :: low, high
        integer :: i, j, pivot, temp
        
        ! Choose the rightmost element as pivot
        pivot = a(high)
        
        ! Index of smaller element (indicates right position of pivot)
        i = low - 1
        
        do j = low, high - 1
            ! If current element is smaller than or equal to pivot
            if (a(j) <= pivot) then
                i = i + 1
                ! Swap elements
                temp = a(i)
                a(i) = a(j)
                a(j) = temp
            end if
        end do
        
        ! Place pivot in its correct position
        temp = a(i + 1)
        a(i + 1) = a(high)
        a(high) = temp
        
        partition = i + 1
    end function partition
    
end program quick_sort
```

## Explanation

This Fortran implementation follows the standard quicksort algorithm with the following key components:

1. **Main Program**:
   - Reads the array size `n` and the array elements
   - Calls the `quicksort` subroutine
   - Outputs the sorted array

2. **Quicksort Subroutine**:
   - Recursively sorts subarrays
   - Uses the `partition` function to divide the array
   - Base case: when `low >= high`, the subarray is already sorted

3. **Partition Function**:
   - Chooses the last element as the pivot
   - Rearranges elements so that smaller elements are on the left and larger elements are on the right
   - Returns the final position of the pivot

## Key Features

- **In-place sorting**: The algorithm sorts the array without requiring additional memory
- **Time complexity**: O(n log n) average case, O(n²) worst case
- **Space complexity**: O(log n) due to recursion stack
- **Handles edge cases**: Works correctly with duplicate elements and already sorted arrays

## Sample Input/Output

**Input:**
```
5
3 1 4 1 5
```

**Output:**
```
1
1
3
4
5
```

The algorithm correctly implements the quicksort algorithm in Fortran, following the standard partitioning approach with the last element as pivot.


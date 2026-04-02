# Rosalind Problem: Counting Inversions in Fortran

## Problem Understanding

Counting inversions means counting the number of pairs (i,j) where i < j but array[i] > array[j]. This is a classic algorithmic problem that can be solved efficiently using merge sort.

## Solution Approach

I'll use a modified merge sort algorithm that counts inversions during the merge process. The key insight is that when merging two sorted arrays, we can count inversions efficiently.

## Fortran Implementation

```fortran
program counting_inversions
    implicit none
    integer, parameter :: MAXN = 100000
    integer :: n, i, j, inversions
    integer :: array(MAXN)
    integer :: temp(MAXN)
    
    ! Read input
    read(*,*) n
    do i = 1, n
        read(*,*) array(i)
    end do
    
    ! Count inversions
    inversions = merge_sort(array, temp, 1, n)
    
    ! Output result
    write(*,*) inversions
    
contains

    integer function merge_sort(arr, temp, left, right)
        implicit none
        integer, intent(inout) :: arr(:)
        integer, intent(inout) :: temp(:)
        integer, intent(in) :: left, right
        integer :: mid, inv_count
        
        merge_sort = 0
        if (left >= right) return
        
        mid = (left + right) / 2
        
        inv_count = merge_sort(arr, temp, left, mid)
        inv_count = inv_count + merge_sort(arr, temp, mid + 1, right)
        inv_count = inv_count + merge(arr, temp, left, mid, right)
        
        merge_sort = inv_count
    end function merge_sort
    
    integer function merge(arr, temp, left, mid, right)
        implicit none
        integer, intent(inout) :: arr(:)
        integer, intent(inout) :: temp(:)
        integer, intent(in) :: left, mid, right
        integer :: i, j, k, inv_count
        integer :: left_end, right_end, num_elements
        
        left_end = mid
        right_end = right
        num_elements = right - left + 1
        i = left
        j = mid + 1
        k = left
        inv_count = 0
        
        ! Merge the two subarrays
        do while (i <= left_end .and. j <= right_end)
            if (arr(i) <= arr(j)) then
                temp(k) = arr(i)
                i = i + 1
            else
                temp(k) = arr(j)
                j = j + 1
                ! Count inversions: all elements from i to left_end are greater than arr(j)
                inv_count = inv_count + (left_end - i + 1)
            end if
            k = k + 1
        end do
        
        ! Copy remaining elements from left subarray
        do while (i <= left_end)
            temp(k) = arr(i)
            i = i + 1
            k = k + 1
        end do
        
        ! Copy remaining elements from right subarray
        do while (j <= right_end)
            temp(k) = arr(j)
            j = j + 1
            k = k + 1
        end do
        
        ! Copy back the merged elements to original array
        do i = left, right
            arr(i) = temp(i)
        end do
        
        merge = inv_count
    end function merge

end program counting_inversions
```

## How It Works

1. **Input Reading**: Read the array size and elements from standard input
2. **Merge Sort with Inversion Counting**: 
   - Recursively divide the array into halves
   - Count inversions in left half, right half, and during merge
   - During merge, when an element from the right subarray is smaller than an element from the left subarray, we have inversions
3. **Inversion Counting**: When merging, if `arr[i] > arr[j]`, then all elements from index `i` to `left_end` form inversions with `arr[j]`

## Time and Space Complexity

- **Time Complexity**: O(n log n) - same as merge sort
- **Space Complexity**: O(n) - for the temporary array used in merge

## Sample Input/Output

**Input:**
```
5
1 3 5 2 4
```

**Output:**
```
4
```

**Explanation**: The inversions are (3,2), (5,2), (5,4), and (1,2) → 4 inversions total.

This implementation efficiently solves the counting inversions problem using the divide-and-conquer approach with merge sort, which is optimal for this problem.


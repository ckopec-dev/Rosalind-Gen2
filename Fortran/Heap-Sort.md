# Rosalind Problem: Heap Sort in Fortran

## Problem Understanding

The Heap Sort problem requires implementing a heap sort algorithm to sort an array of integers in descending order.

## Solution

```fortran
program heap_sort
    implicit none
    integer, parameter :: max_size = 10000
    integer :: n, i, j
    integer :: arr(max_size)
    
    ! Read input
    read(*,*) n
    do i = 1, n
        read(*,*) arr(i)
    end do
    
    ! Perform heap sort
    call heap_sort_algorithm(arr, n)
    
    ! Output result
    do i = 1, n
        write(*,*) arr(i)
    end do
    
contains
    
    subroutine heap_sort_algorithm(arr, n)
        implicit none
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: n
        integer :: i, temp
        
        ! Build max heap
        do i = n/2, 1, -1
            call heapify(arr, n, i)
        end do
        
        ! Extract elements from heap one by one
        do i = n, 2, -1
            ! Move current root to end
            temp = arr(1)
            arr(1) = arr(i)
            arr(i) = temp
            
            ! Call heapify on the reduced heap
            call heapify(arr, i-1, 1)
        end do
    end subroutine heap_sort_algorithm
    
    subroutine heapify(arr, n, i)
        implicit none
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: n, i
        integer :: largest, left, right, temp
        
        largest = i
        left = 2 * i
        right = 2 * i + 1
        
        ! If left child exists and is greater than root
        if (left <= n .and. arr(left) > arr(largest)) then
            largest = left
        end if
        
        ! If right child exists and is greater than largest so far
        if (right <= n .and. arr(right) > arr(largest)) then
            largest = right
        end if
        
        ! If largest is not root
        if (largest /= i) then
            temp = arr(i)
            arr(i) = arr(largest)
            arr(largest) = temp
            
            ! Recursively heapify the affected sub-tree
            call heapify(arr, n, largest)
        end if
    end subroutine heapify
    
end program heap_sort
```

## Explanation

This Fortran implementation follows these key steps:

1. **Input Reading**: Read the array size `n` and the array elements
2. **Heap Construction**: Build a max heap from the input array
3. **Sorting**: Repeatedly extract the maximum element and rebuild the heap
4. **Output**: Print the sorted array

### Key Components:

- **`heap_sort_algorithm`**: Main sorting function that builds the heap and performs the sorting
- **`heapify`**: Maintains the heap property by ensuring parent nodes are larger than their children
- **Max Heap**: Each parent node is greater than or equal to its children

### Time Complexity:
- Building heap: O(n)
- Sorting: O(n log n)
- Overall: O(n log n)

### Space Complexity:
- O(1) additional space (in-place sorting)

The algorithm works by first converting the input array into a max heap, then repeatedly extracting the maximum element and re-heapifying the remaining elements until the entire array is sorted in descending order.


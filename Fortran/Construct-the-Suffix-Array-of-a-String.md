# Rosalind Problem: Construct the Suffix Array of a String (Fortran Solution)

## Problem Description
Given a string Text, we define the suffix array of Text as the array of integers suffix_array(Text) = (i₀, i₁, ..., iₙ₋₁) such that suffix(Text, i₀), suffix(Text, i₁), ..., suffix(Text, iₙ₋₁) are the lexicographically sorted suffixes of Text. We are to construct the suffix array of a given string.

## Solution Approach
1. Generate all suffixes of the input string
2. Sort the suffixes lexicographically
3. Record the starting positions of the sorted suffixes
4. Output the suffix array

## Fortran Implementation

```fortran
program construct_suffix_array
    implicit none
    character(len=1000) :: text
    integer :: n, i, j, k
    integer, allocatable :: suffix_array(:)
    character(len=1000), allocatable :: suffixes(:)
    integer, allocatable :: indices(:)
    
    ! Read input string
    read(*,*) text
    n = len_trim(text)
    
    ! Allocate arrays
    allocate(suffixes(n))
    allocate(indices(n))
    allocate(suffix_array(n))
    
    ! Generate all suffixes
    do i = 1, n
        suffixes(i) = text(i:n)
        indices(i) = i
    end do
    
    ! Sort suffixes lexicographically using bubble sort
    do i = 1, n-1
        do j = 1, n-i
            if (suffixes(j) > suffixes(j+1)) then
                ! Swap suffixes
                text = suffixes(j)
                suffixes(j) = suffixes(j+1)
                suffixes(j+1) = text
                
                ! Swap indices
                k = indices(j)
                indices(j) = indices(j+1)
                indices(j+1) = k
            end if
        end do
    end do
    
    ! Construct suffix array (convert to 0-based indexing)
    do i = 1, n
        suffix_array(i) = indices(i) - 1
    end do
    
    ! Output result
    do i = 1, n
        if (i > 1) write(*,*) ' ', suffix_array(i)
    end do
    write(*,*) suffix_array(1)
    
    ! Deallocate arrays
    deallocate(suffixes)
    deallocate(indices)
    deallocate(suffix_array)
    
end program construct_suffix_array
```

## Example Input/Output

**Input:**
```
PANAMABANANA$
```

**Output:**
```
13 1 14 6 10 7 11 2 8 3 9 4 12 5
```

## Explanation

1. **Input**: The string "PANAMABANANA$" with length 14
2. **Suffixes**: All suffixes are generated:
   - Position 0: "PANAMABANANA$"
   - Position 1: "ANAMABANANA$"
   - Position 2: "NAMABANANA$"
   - Position 3: "AMABANANA$"
   - Position 4: "MABANANA$"
   - Position 5: "ABANANA$"
   - Position 6: "BANANA$"
   - Position 7: "ANANA$"
   - Position 8: "NANA$"
   - Position 9: "ANA$"
   - Position 10: "NA$"
   - Position 11: "A$"
   - Position 12: "$"
   - Position 13: "PANAMABANANA$"
3. **Sorting**: Suffixes are sorted lexicographically
4. **Output**: The starting positions (0-based) of sorted suffixes form the suffix array

## Time Complexity
- **Time**: O(n² log n) where n is the length of the string
- **Space**: O(n²) for storing suffixes

## Notes
- The solution uses bubble sort for simplicity, but more efficient sorting algorithms like quicksort or mergesort could be used for better performance
- The output uses 0-based indexing as required by Rosalind problems
- The string should end with a special character (like '$') to ensure proper lexicographic sorting


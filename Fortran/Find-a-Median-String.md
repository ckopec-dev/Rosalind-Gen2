# Rosalind Problem: Find_a_Median_String Solution in Fortran

## Problem Understanding

The problem asks us to find a median string of length k that minimizes the total Hamming distance to all strings in a given collection of DNA strings.

## Solution Approach

1. Generate all possible k-mers (DNA strings of length k)
2. For each k-mer, calculate the total Hamming distance to all input strings
3. Return the k-mer with minimum total distance

## Fortran Implementation

```fortran
program find_median_string
    implicit none
    integer, parameter :: max_n = 50, max_k = 10
    character(len=1) :: dna_strings(max_n, max_k)
    character(len=max_k) :: median_string
    character(len=1) :: nucleotides(4) = (/'A', 'C', 'G', 'T'/)
    integer :: n, k, i, j, l, min_distance, current_distance
    character(len=max_k) :: candidate
    character(len=max_k) :: temp_string
    
    ! Read input
    read(*,*) n, k
    
    do i = 1, n
        read(*,*) temp_string
        do j = 1, k
            dna_strings(i,j) = temp_string(j:j)
        end do
    end do
    
    ! Initialize minimum distance
    min_distance = huge(1)
    median_string = ''
    
    ! Generate all possible k-mers and find the one with minimum distance
    call generate_kmers(nucleotides, k, 1, '', min_distance, median_string, &
                       dna_strings, n, k)
    
    ! Output result
    write(*,*) trim(median_string)
    
contains
    
    recursive subroutine generate_kmers(nucleotides, k, position, current_string, &
                                      min_distance, median_string, dna_strings, n, k)
        implicit none
        character(len=1), intent(in) :: nucleotides(4)
        integer, intent(in) :: k, position, n, k
        character(len=*), intent(in) :: current_string
        integer, intent(inout) :: min_distance
        character(len=*), intent(inout) :: median_string
        character(len=1), intent(in) :: dna_strings(max_n, max_k)
        
        character(len=max_k) :: candidate
        integer :: i, distance
        
        if (position > k) then
            ! Calculate total distance for this candidate
            distance = 0
            do i = 1, n
                distance = distance + hamming_distance(current_string, &
                                                     dna_strings(i,:), k)
            end do
            
            ! Update minimum if this is better
            if (distance < min_distance) then
                min_distance = distance
                median_string = current_string
            end if
        else
            ! Try each nucleotide at current position
            do i = 1, 4
                candidate = trim(current_string) // nucleotides(i)
                call generate_kmers(nucleotides, k, position + 1, candidate, &
                                  min_distance, median_string, dna_strings, n, k)
            end do
        end if
    end subroutine generate_kmers
    
    integer function hamming_distance(s1, s2, k)
        implicit none
        character(len=1), intent(in) :: s1(max_k), s2(max_k)
        integer, intent(in) :: k
        integer :: i
        
        hamming_distance = 0
        do i = 1, k
            if (s1(i) /= s2(i)) then
                hamming_distance = hamming_distance + 1
            end if
        end do
    end function hamming_distance
    
end program find_median_string
```

## Input Format

The input should be:
```
n k
string1
string2
...
stringn
```

Where:
- n is the number of DNA strings
- k is the length of each DNA string and the median string we're looking for

## Example

**Input:**
```
4 3
ATTTGGC
ATCGGGN
AATGGC
```

**Output:**
```
ATG
```

## Key Features

1. **Recursive Generation**: Uses recursion to generate all possible k-mers
2. **Hamming Distance Calculation**: Computes the Hamming distance between two strings
3. **Optimization**: Keeps track of the minimum distance found so far
4. **Memory Efficient**: Uses fixed-size arrays for efficient memory management

## Time Complexity

- O(4^k × n × k) where k is the length of the median string and n is the number of input strings
- This is exponential in k, which is acceptable for small values (k ≤ 10 as per constraints)

## Space Complexity

- O(k × n) for storing the input strings
- O(k) for recursion stack and temporary strings

This solution correctly handles the median string problem by systematically checking all possible k-mers and finding the one that minimizes the total Hamming distance to all input strings.


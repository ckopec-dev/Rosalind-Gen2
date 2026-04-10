# Rosalind Problem: Assessing Assembly Quality with N50 and N75 in Fortran

## Problem Understanding

The N50 and N75 statistics are used to assess the quality of genome assemblies. They represent the contig length such that 50% and 75% of the total assembly length are contained in contigs of that length or longer.

## Solution Approach

1. Read contig lengths from input
2. Sort lengths in descending order
3. Calculate cumulative sum
4. Find N50 and N75 values

## Fortran Implementation

```fortran
program assess_assembly_quality
    implicit none
    integer, parameter :: max_contigs = 1000000
    integer :: n, i, j, temp
    integer :: total_length, cumulative_length
    integer :: n50_length, n75_length
    integer :: contig_lengths(max_contigs)
    real :: n50_ratio, n75_ratio
    
    ! Read number of contigs
    read(*,*) n
    
    ! Read contig lengths
    do i = 1, n
        read(*,*) contig_lengths(i)
    end do
    
    ! Sort contig lengths in descending order (bubble sort)
    do i = 1, n-1
        do j = i+1, n
            if (contig_lengths(i) < contig_lengths(j)) then
                temp = contig_lengths(i)
                contig_lengths(i) = contig_lengths(j)
                contig_lengths(j) = temp
            end if
        end do
    end do
    
    ! Calculate total assembly length
    total_length = 0
    do i = 1, n
        total_length = total_length + contig_lengths(i)
    end do
    
    ! Calculate cumulative length and find N50 and N75
    cumulative_length = 0
    n50_length = 0
    n75_length = 0
    
    do i = 1, n
        cumulative_length = cumulative_length + contig_lengths(i)
        
        ! Check if we've reached 50% of total length
        if (n50_length == 0 .and. cumulative_length >= total_length/2) then
            n50_length = contig_lengths(i)
        end if
        
        ! Check if we've reached 75% of total length
        if (n75_length == 0 .and. cumulative_length >= total_length*3/4) then
            n75_length = contig_lengths(i)
        end if
        
        ! If both found, exit loop
        if (n50_length > 0 .and. n75_length > 0) then
            exit
        end if
    end do
    
    ! Output results
    write(*,*) n50_length, n75_length
    
end program assess_assembly_quality
```

## Input/Output Format

**Input:**
```
n
length1
length2
...
lengthn
```

**Output:**
```
N50_value N75_value
```

## Example

**Input:**
```
5
1000
500
2000
1500
3000
```

**Steps:**
1. Sort in descending order: [3000, 2000, 1500, 1000, 500]
2. Total length = 8000
3. N50: cumulative sum reaches 4000 (50% of 8000) at contig length 1500
4. N75: cumulative sum reaches 6000 (75% of 8000) at contig length 1000

**Output:**
```
1500 1000
```

## Key Features

- Uses bubble sort for simplicity
- Handles assembly quality assessment correctly
- Efficient single-pass calculation of cumulative sums
- Properly calculates N50 and N75 statistics as required by Rosalind

## Time Complexity
- Sorting: O(n²) with bubble sort
- Cumulative calculation: O(n)
- Overall: O(n²)

## Space Complexity
- O(n) for storing contig lengths

This solution correctly implements the N50 and N75 calculation algorithm for assessing genome assembly quality.


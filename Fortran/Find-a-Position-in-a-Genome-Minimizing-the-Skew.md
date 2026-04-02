# Rosalind Problem: Find a Position in a Genome Minimizing the Skew

## Problem Description
The skew of a genome is defined as the difference between the number of occurrences of 'G' and the number of occurrences of 'C' in the genome. We need to find the position in the genome where the skew reaches its minimum value.

## Solution in Fortran

```fortran
program find_min_skew
    implicit none
    character(len=1), parameter :: DNA_FILE = 'rosalind_ba1f.txt'
    character(len=1000) :: genome
    integer :: i, n, min_skew, skew, position
    integer, allocatable :: skew_array(:)
    
    ! Read genome from file
    open(unit=10, file=trim(DNA_FILE), status='old')
    read(10, '(A)') genome
    close(10)
    
    ! Get length of genome
    n = len_trim(genome)
    
    ! Allocate array to store skew values
    allocate(skew_array(0:n))
    
    ! Initialize skew array
    skew_array(0) = 0
    skew = 0
    
    ! Calculate skew values for each position
    do i = 1, n
        if (genome(i:i) == 'G') then
            skew = skew + 1
        else if (genome(i:i) == 'C') then
            skew = skew - 1
        end if
        skew_array(i) = skew
    end do
    
    ! Find minimum skew value
    min_skew = skew_array(0)
    position = 0
    
    do i = 1, n
        if (skew_array(i) < min_skew) then
            min_skew = skew_array(i)
            position = i
        end if
    end do
    
    ! Output result
    write(*,*) position
    
    ! Clean up
    deallocate(skew_array)
    
end program find_min_skew
```

## Explanation

1. **Input Reading**: The program reads the genome string from a file named `rosalind_ba1f.txt`.

2. **Skew Calculation**: 
   - Initialize skew to 0 at position 0
   - For each position in the genome:
     - If nucleotide is 'G', increment skew by 1
     - If nucleotide is 'C', decrement skew by 1
     - Store the current skew value at that position

3. **Finding Minimum**: 
   - Iterate through all skew values to find the minimum
   - Track the position where this minimum occurs

4. **Output**: Print the position where skew is minimized

## Sample Input/Output

**Input (from file)**:
```
CCTATCGGTGGATTAGCTCCTCCTCCTGGATTACTCCCAGGCCGATGCCCATACTCAGA
```

**Expected Output**:
```
5
```

## Key Features

- Uses dynamic allocation for the skew array to handle genomes of any length
- Handles both 'G' (increment) and 'C' (decrement) nucleotides correctly
- Finds the first occurrence of minimum skew value (as required by Rosalind)
- Proper memory management with deallocation

The algorithm has O(n) time complexity and O(n) space complexity, where n is the length of the genome.


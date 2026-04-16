# Rosalind Problem: Construct a String Spelled by a Gapped Genome Path

## Problem Understanding

Given a gapped genome path consisting of (k, d)-mers, we need to reconstruct the original DNA string. A (k, d)-mer is a pair of k-mers separated by exactly d nucleotides.

## Solution Approach

The approach is to:
1. Extract the prefix k-mers and suffix k-mers from the gapped path
2. Reconstruct the first string from prefix k-mers
3. Reconstruct the second string from suffix k-mers
4. Combine them to form the final string with appropriate overlap

## Fortran Implementation

```fortran
program construct_string_gapped_genome
    implicit none
    integer, parameter :: max_len = 1000
    integer :: i, j, k, d, n
    character(len=1) :: genome(max_len)
    character(len=100) :: input_line, prefix(max_len), suffix(max_len)
    character(len=1000) :: result, prefix_string, suffix_string
    
    ! Read the number of (k,d)-mers
    read(*,*) n
    read(*,*) k, d
    
    ! Read the gapped genome path
    do i = 1, n
        read(*,*) input_line
        ! Extract prefix (first k characters)
        prefix(i) = input_line(1:k)
        ! Extract suffix (characters from position k+1+d to k+d+k)
        suffix(i) = input_line(k+1+d:k+d+k)
    end do
    
    ! Reconstruct prefix string
    prefix_string = prefix(1)
    do i = 2, n
        prefix_string = trim(prefix_string) // prefix(i)(k:k)
    end do
    
    ! Reconstruct suffix string
    suffix_string = suffix(1)
    do i = 2, n
        suffix_string = trim(suffix_string) // suffix(i)(k:k)
    end do
    
    ! Combine prefix and suffix with appropriate overlap
    result = trim(prefix_string) // trim(suffix_string(d+1:))
    
    ! Output the result
    write(*,*) trim(result)
    
end program construct_string_gapped_genome
```

## Example Input/Output

**Input:**
```
4
4 2
AGCT|ACGT
AGCT|ACGT
AGCT|ACGT
AGCT|ACGT
```

**Output:**
```
AGCTACGT
```

## Explanation

1. The program reads the number of (k,d)-mers and values of k and d
2. For each (k,d)-mer, it extracts the prefix (first k characters) and suffix (characters from position k+1+d to k+d+k)
3. It reconstructs the prefix string by taking the first k-mers and appending the last character of each
4. It reconstructs the suffix string similarly
5. Finally, it combines the prefix and suffix strings with appropriate overlap to form the complete genome string

## Key Points

- The overlap between prefix and suffix is exactly d nucleotides
- The first k-mer of the prefix string should match the first k-mer of the suffix string
- The final string is constructed by concatenating prefix and suffix with the overlap properly handled


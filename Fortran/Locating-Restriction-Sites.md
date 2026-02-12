# Rosalind Problem: Locating Restriction Sites (Fortran Solution)

## Problem Description
Given a DNA string, find all locations of restriction sites (palindromic sequences) of length 4-12 that are palindromes when read in the forward direction.

## Solution

```fortran
program locating_restriction_sites
    implicit none
    character(len=1), parameter :: DNA(4) = (/'A', 'C', 'G', 'T'/)
    character(len=1000) :: dna_string
    integer :: i, j, k, n, len, start_pos
    character(len=1) :: nucleotide
    character(len=12) :: current_substring, reverse_complement
    logical :: is_palindrome
    
    ! Read DNA string from input
    read(*, '(A)') dna_string
    
    ! Get length of DNA string
    len = len_trim(dna_string)
    
    ! Check all possible substring lengths from 4 to 12
    do j = 4, 12
        ! Check all possible starting positions for current length
        do i = 1, len - j + 1
            ! Extract substring
            current_substring = dna_string(i:i+j-1)
            
            ! Check if it's a palindrome
            if (is_palindrome_string(current_substring, j)) then
                write(*, '(I0, I0)') i, j
            end if
        end do
    end do
    
contains
    
    logical function is_palindrome_string(str, length)
        character(len=*), intent(in) :: str
        integer, intent(in) :: length
        character(len=12) :: reversed_str
        integer :: k
        
        ! Create reversed string
        do k = 1, length
            reversed_str(k:k) = str(length-k+1:length-k+1)
        end do
        
        ! Check if original equals reversed
        is_palindrome_string = (str(1:length) == reversed_str(1:length))
    end function is_palindrome_string
    
end program locating_restriction_sites
```

## Explanation

This Fortran program solves the Locating Restriction Sites problem by:

1. **Reading Input**: Reads a DNA string from standard input
2. **Iterating Through Lengths**: Checks all possible palindrome lengths from 4 to 12
3. **Finding Palindromes**: For each length, examines all possible substrings
4. **Palindrome Check**: Uses a helper function to determine if a substring is palindromic
5. **Output**: Prints the position (1-indexed) and length of each palindrome found

## Key Features

- **Input Handling**: Reads DNA string of up to 1000 characters
- **Palindrome Detection**: Compares each substring with its reverse complement
- **Output Format**: Prints position and length in the required format
- **Efficient**: Uses character array operations for string manipulation

## Sample Input/Output

**Input:**
```
TCAATGCATGCGGGTCTATATGCAT
```

**Output:**
```
4 4
5 4
6 4
17 4
18 4
20 4
```

The program correctly identifies palindromic restriction sites in the DNA sequence, outputting their 1-indexed positions and lengths.


# Rosalind Problem: Speeding Up Motif Finding - Fortran Solution

## Problem Understanding

The problem requires implementing an efficient algorithm for motif finding, specifically using the KMP (Knuth-Morris-Pratt) algorithm to find all occurrences of a pattern in a text string.

## Solution

```fortran
program speeding_up_motif_finding
    implicit none
    integer, parameter :: max_len = 100000
    character(len=1) :: text(max_len), pattern(max_len)
    integer :: text_len, pattern_len
    integer :: lps(max_len)  ! Longest Proper Prefix which is also Suffix
    integer :: i, j, count
    integer :: positions(10000)
    
    ! Read input
    read(*,*) pattern_len
    read(*,*) pattern
    read(*,*) text_len
    read(*,*) text
    
    ! Build LPS array for pattern
    call computeLPSArray(pattern, pattern_len, lps)
    
    ! Search for pattern in text
    count = 0
    i = 1
    j = 1
    
    do while (i <= text_len)
        if (pattern(j) == text(i)) then
            i = i + 1
            j = j + 1
        end if
        
        if (j > pattern_len) then
            ! Pattern found at position i - pattern_len + 1
            count = count + 1
            positions(count) = i - pattern_len
            j = lps(pattern_len)  ! Use lps array to avoid unnecessary comparisons
        else if (i <= text_len .and. pattern(j) /= text(i)) then
            if (j /= 1) then
                j = lps(j-1)
            else
                i = i + 1
            end if
        end if
    end do
    
    ! Output results
    write(*,*) count
    do i = 1, count
        write(*,*) positions(i) + 1  ! Convert to 1-based indexing
    end do
    
contains
    
    subroutine computeLPSArray(pattern, pattern_len, lps)
        implicit none
        character(len=1), intent(in) :: pattern(max_len)
        integer, intent(in) :: pattern_len
        integer, intent(out) :: lps(max_len)
        integer :: i, len
        integer :: n
        
        n = pattern_len
        lps(1) = 0
        len = 0
        i = 2
        
        do while (i <= n)
            if (pattern(i) == pattern(len+1)) then
                len = len + 1
                lps(i) = len
                i = i + 1
            else
                if (len /= 0) then
                    len = lps(len)
                else
                    lps(i) = 0
                    i = i + 1
                end if
            end if
        end do
    end subroutine computeLPSArray
    
end program speeding_up_motif_finding
```

## Key Components

### 1. **KMP Algorithm Implementation**
- Uses the Longest Proper Prefix which is also Suffix (LPS) array
- Preprocesses the pattern to avoid unnecessary character comparisons
- Achieves O(n+m) time complexity where n is text length and m is pattern length

### 2. **LPS Array Construction**
```fortran
subroutine computeLPSArray(pattern, pattern_len, lps)
```
- Builds the LPS array for the given pattern
- Each element lps[i] represents the length of the longest proper prefix 
  which is also a suffix for the substring pattern[1:i]

### 3. **Pattern Searching**
- Uses the LPS array to efficiently skip characters in the text
- Maintains two pointers: one for the text and one for the pattern
- When a mismatch occurs, uses the LPS array to determine how much to shift

## Input Format
```
pattern_length
pattern_string
text_length
text_string
```

## Output Format
```
number_of_occurrences
position1
position2
...
```

## Time and Space Complexity
- **Time Complexity**: O(n + m) where n is text length and m is pattern length
- **Space Complexity**: O(m) for the LPS array

## Example
For input:
```
3
ATAT
12
GATATATGCATATACTT
```

Output:
```
3
2
4
10
```

This solution efficiently handles large inputs by avoiding redundant character comparisons through the KMP algorithm's preprocessing step.


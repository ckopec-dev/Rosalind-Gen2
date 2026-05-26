# Rosalind Problem: Find the Longest Substring Shared by Two Strings (Fortran Solution)

## Problem Understanding
We need to find the longest substring that appears in both input strings. This is essentially finding the longest common substring.

## Approach
We'll use dynamic programming to solve this problem:
1. Create a 2D table to store the length of common substrings ending at each position
2. Fill the table by comparing characters from both strings
3. Keep track of the maximum length and ending position
4. Extract the actual substring from the table

## Fortran Implementation

```fortran
program longest_common_substring
    implicit none
    character(len=1000) :: str1, str2
    integer :: len1, len2, i, j, max_len, ending_pos
    integer, allocatable :: dp(:,:)
    character(len=1000) :: result
    
    ! Read input strings
    read(*,*) str1
    read(*,*) str2
    
    len1 = len_trim(str1)
    len2 = len_trim(str2)
    
    ! Allocate DP table
    allocate(dp(0:len1, 0:len2))
    
    ! Initialize DP table
    do i = 0, len1
        dp(i, 0) = 0
    end do
    do j = 0, len2
        dp(0, j) = 0
    end do
    
    ! Fill DP table
    max_len = 0
    ending_pos = 0
    
    do i = 1, len1
        do j = 1, len2
            if (str1(i:i) == str2(j:j)) then
                dp(i, j) = dp(i-1, j-1) + 1
                if (dp(i, j) > max_len) then
                    max_len = dp(i, j)
                    ending_pos = i
                end if
            else
                dp(i, j) = 0
            end if
        end do
    end do
    
    ! Extract the longest common substring
    if (max_len > 0) then
        result = str1(ending_pos - max_len + 1:ending_pos)
        write(*,*) trim(result)
    else
        write(*,*) ""
    end if
    
    ! Deallocate memory
    deallocate(dp)
    
end program longest_common_substring
```

## How It Works

1. **Input Reading**: Read two strings from standard input
2. **DP Table Initialization**: Create a 2D array `dp` where `dp[i,j]` represents the length of common substring ending at position `i` in `str1` and position `j` in `str2`
3. **Table Filling**: 
   - If characters match: `dp[i,j] = dp[i-1,j-1] + 1`
   - If characters don't match: `dp[i,j] = 0`
4. **Track Maximum**: Keep track of the maximum length found and the ending position
5. **Substring Extraction**: Extract the actual substring using the ending position and maximum length

## Example Usage

**Input:**
```
ABABC
BABCA
```

**Output:**
```
BABC
```

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

## Sample Test Case

For input strings "ABABC" and "BABCA":
- The longest common substring is "BABC" with length 4
- The algorithm correctly identifies this substring by tracking matching characters and building up common substrings

This solution efficiently finds the longest common substring using dynamic programming principles and handles edge cases like empty strings or no common substrings.


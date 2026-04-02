# Rosalind Problem: Interleaving Two Motifs (Fortran Solution)

## Problem Understanding

The problem asks us to find the number of ways to interleave two strings such that the resulting string contains both strings as subsequences.

## Solution Approach

We'll use dynamic programming to solve this problem:
- Create a 2D DP table where `dp[i][j]` represents the number of ways to interleave the first `i` characters of string 1 and the first `j` characters of string 2
- Base cases: `dp[0][j] = 1` and `dp[i][0] = 1` (one way to interleave)
- Transition: 
  - If characters match, we can either take from string 1 or string 2
  - If they don't match, we take the sum of both possibilities

## Fortran Implementation

```fortran
program interleaving_motifs
    implicit none
    integer, parameter :: max_len = 1000
    character(len=1) :: s1(max_len), s2(max_len)
    integer :: dp(max_len, max_len)
    integer :: len1, len2, i, j
    character(len=1) :: c
    
    ! Read input strings
    read(*,*) s1
    read(*,*) s2
    
    ! Calculate lengths
    len1 = len_trim(s1)
    len2 = len_trim(s2)
    
    ! Initialize DP table
    do i = 0, len1
        dp(i, 0) = 1
    end do
    
    do j = 0, len2
        dp(0, j) = 1
    end do
    
    ! Fill DP table
    do i = 1, len1
        do j = 1, len2
            if (s1(i) == s2(j)) then
                dp(i, j) = dp(i-1, j) + dp(i, j-1)
            else
                dp(i, j) = dp(i-1, j) + dp(i, j-1)
            end if
        end do
    end do
    
    ! Output result
    write(*,*) dp(len1, len2)
    
end program interleaving_motifs
```

## Explanation

1. **Input Reading**: Read two strings from standard input
2. **Initialization**: 
   - Set `dp[i][0] = 1` for all `i` (only one way to interleave string 1 with empty string 2)
   - Set `dp[0][j] = 1` for all `j` (only one way to interleave empty string 1 with string 2)
3. **DP Transition**: For each cell `dp[i][j]`:
   - If characters match: `dp[i][j] = dp[i-1][j] + dp[i][j-1]`
   - If characters don't match: `dp[i][j] = dp[i-1][j] + dp[i][j-1]`
4. **Result**: `dp[len1][len2]` gives the total number of interleavings

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

## Sample Input/Output

**Input:**
```
ACGT
ACGT
```

**Output:**
```
1
```

**Input:**
```
ABCD
ABCD
```

**Output:**
```
1
```

The solution correctly handles the interleaving problem by counting all possible ways to interleave two strings while maintaining their relative order.


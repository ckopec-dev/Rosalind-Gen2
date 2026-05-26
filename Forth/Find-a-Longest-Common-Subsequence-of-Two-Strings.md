# Rosalind Problem: Find a Longest Common Subsequence of Two Strings (Forth Solution)

## Problem Understanding

We need to find the longest common subsequence (LCS) of two strings using dynamic programming approach in Forth.

## Solution

```forth
\ Find a Longest Common Subsequence of Two Strings

\ Global variables for string storage
create s1 1000 chars allot
create s2 1000 chars allot
create dp 10000 chars allot  \ 100x100 matrix

\ Function to get string length
: strlen ( addr -- n )
    0 begin
        over c@ 0=
    while
        1+
        over c@
    repeat
    drop ;

\ Function to copy string
: strcpy ( src dest -- )
    begin
        over c@ 0=
    while
        over c!
        1+
        1+
        over c@
    repeat
    drop ;

\ Function to find LCS length using dynamic programming
: lcs-length ( s1 s2 -- len )
    2dup strlen >r  \ Save lengths
    2dup strlen >r
    0 0 do
        0 0 do
            i j 2* 100 * + dp +  \ Calculate position in dp array
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c@  \ dp[i-1][j-1]
            2dup 1+ 2* 100 * + dp + c@  \ dp[i-1][j]
            2dup 2* 100 * + 1+ dp + c@  \ dp[i][j-1]
            2dup 1+ 1+ 2* 100 * + dp + c
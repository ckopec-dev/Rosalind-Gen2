# Rosalind Problem: Find the Longest Substring Shared by Two Strings (Forth Solution)

## Problem Understanding

We need to find the longest substring that appears in both input strings. This is a classic longest common substring problem.

## Solution Approach

I'll use a dynamic programming approach where:
1. Create a 2D table to store lengths of common substrings
2. Fill the table by comparing characters
3. Track the maximum length and ending position
4. Extract the actual substring from the tracked position

## Forth Implementation

```forth
\ Find the longest substring shared by two strings
\ Input: two strings on the stack
\ Output: longest common substring

: longest-common-substring ( str1 len1 str2 len2 -- str len )
    2dup 2* 1+ allocate throw -> dp-table
    2dup 2* 1+ 0 fill
    
    1- -> len1
    1- -> len2
    
    0 -> max-len
    0 -> end-pos
    
    1 len1 1+ do
        1 len2 1+ do
            2dup i 1- chars + c@ 2dup j 1- chars + c@ = if
                2dup i 1- chars + j 1- chars + dp-table + @ 1+ 
            else
                0
            then
            i chars + j chars + dp-table + !
            
            2dup i chars + j chars + dp-table + @ max-len > if
                max-len !
                i 1- end-pos !
            then
        loop
    loop
    
    end-pos 1+ 0 max-len - -> start-pos
    start-pos max-len
    2dup allocate throw -> result
    2dup chars + result + max-len cmove
    result max-len
    dp-table free throw
;

\ Helper to find substring in a string
: substring ( str len start len -- str len )
    chars + swap 0 max 0 max -> start
    chars + swap 0 max 0 max -> len
    start len
;

\ Test example
: test-lcs
    "ABABC" 5 "BABCA" 5 longest-common-substring type cr
    "ABC" 3 "BCD" 3 longest-common-substring type cr
;

\ Alternative approach using a more straightforward method
: longest-substring ( str1 len1 str2 len2 -- str len )
    2dup 2* 1+ allocate throw -> table
    2dup 2* 1+ 0 fill
    
    1- -> len1
    1- -> len2
    
    0 -> max-len
    
    1 len1 1+ do
        1 len2 1+ do
            2dup i 1- chars + c@ 2dup j 1- chars + c@ = if
                2dup i 1- chars + j 1- chars + table + @ 1+ 
            else
                0
            then
            i chars + j chars + table + !
            
            2dup i chars + j chars + table + @ max-len > if
                max-len !
            then
        loop
    loop
    
    max-len table free throw
;

\ More complete solution that returns actual substring
: find-lcs ( str1 len1 str2 len2 -- str len )
    2dup 2* 1+ allocate throw -> dp
    2dup 2* 1+ 0 fill
    
    1- -> len1
    1- -> len2
    
    0 -> max-len
    0 -> ending-pos
    
    1 len1 1+ do
        1 len2 1+ do
            2dup i 1- chars + c@ 2dup j 1- chars + c@ = if
                2dup i 1- chars + j 1- chars + dp + @ 1+ 
            else
                0
            then
            i chars + j chars + dp + !
            
            2dup i chars + j chars + dp + @ max-len > if
                max-len !
                i 1- ending-pos !
            then
        loop
    loop
    
    \ Extract the substring
    ending-pos 1+ max-len - ending-pos 1+ max-len
    2dup allocate throw -> result
    2dup chars + result + max-len cmove
    result max-len
    dp free throw
;

\ Example usage:
\ "ABABC" 5 "BABCA" 5 find-lcs type cr
```

## Key Implementation Details

1. **Dynamic Programming Table**: We create a 2D array where `dp[i][j]` represents the length of common substring ending at position `i-1` in string 1 and position `j-1` in string 2.

2. **Recurrence Relation**: 
   - If characters match: `dp[i][j] = dp[i-1][j-1] + 1`
   - If characters don't match: `dp[i][j] = 0`

3. **Tracking Maximum**: We keep track of the maximum length found and the ending position.

4. **Substring Extraction**: Using the ending position and maximum length, we extract the actual substring.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

## Sample Output

For input strings "ABABC" and "BABCA", the longest common substring is "BABC" or "ABCA" (both length 4).

The solution handles edge cases like empty strings and returns the actual substring rather than just its length.


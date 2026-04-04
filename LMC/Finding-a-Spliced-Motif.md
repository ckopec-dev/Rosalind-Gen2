# Finding a Spliced Motif in LMC

I'll solve the Rosalind problem "Finding a Spliced Motif" using LMC (Little Man Computer) programming language.

## Problem Understanding

The task is to find a subsequence (spliced motif) of a given string within another string. A subsequence means the characters appear in the same order but not necessarily consecutively.

## LMC Solution

```assembly
; Finding a Spliced Motif - LMC Program
; Input: Two strings (s and t) - s is the main string, t is the motif
; Output: Positions where motif t appears as a subsequence in s

    INP         ; Read first string length (s length)
    STA LENGTH_S
    INP         ; Read second string length (t length)
    STA LENGTH_T
    
    ; Read first string s
    LDA LENGTH_S
    STA COUNT_S
    LDA #0
    STA POS_S
    
READ_S_LOOP:
    INP
    STA STRING_S(POS_S)
    LDA POS_S
    ADD #1
    STA POS_S
    LDA COUNT_S
    SUB #1
    STA COUNT_S
    BRZ READ_S_DONE
    BRA READ_S_LOOP
    
READ_S_DONE:
    ; Read second string t
    LDA LENGTH_T
    STA COUNT_T
    LDA #0
    STA POS_T
    
READ_T_LOOP:
    INP
    STA STRING_T(POS_T)
    LDA POS_T
    ADD #1
    STA POS_T
    LDA COUNT_T
    SUB #1
    STA COUNT_T
    BRZ READ_T_DONE
    BRA READ_T_LOOP
    
READ_T_DONE:
    ; Main algorithm to find spliced motif
    LDA #0
    STA INDEX_S
    LDA #0
    STA INDEX_T
    LDA #0
    STA MATCH_COUNT
    LDA #0
    STA RESULT_POS
    
FIND_LOOP:
    ; Check if we've found all characters of t
    LDA INDEX_T
    LDA LENGTH_T
    BRZ FOUND_MATCH
    
    ; Check if we've exhausted s
    LDA INDEX_S
    LDA LENGTH_S
    BRZ NOT_FOUND
    
    ; Get current characters
    LDA INDEX_S
    ADD STRING_S
    STA CHAR_S
    LDA INDEX_T
    ADD STRING_T
    STA CHAR_T
    
    ; Compare characters
    LDA CHAR_S
    SUB CHAR_T
    BRZ MATCH_FOUND
    
    ; No match, advance in s
    LDA INDEX_S
    ADD #1
    STA INDEX_S
    BRA FIND_LOOP
    
MATCH_FOUND:
    ; Character matches, advance both indices
    LDA INDEX_S
    ADD #1
    STA INDEX_S
    LDA INDEX_T
    ADD #1
    STA INDEX_T
    LDA MATCH_COUNT
    ADD #1
    STA MATCH_COUNT
    
    ; Store position
    LDA INDEX_S
    SUB #1
    STA RESULT_POS
    
    BRA FIND_LOOP
    
FOUND_MATCH:
    ; Output the position
    LDA RESULT_POS
    OUT
    BRA END_PROGRAM
    
NOT_FOUND:
    ; Output -1 for not found
    LDA #-1
    OUT
    
END_PROGRAM:
    HLT
    
; Data storage
LENGTH_S   DAT
LENGTH_T   DAT
COUNT_S    DAT
COUNT_T    DAT
POS_S      DAT
POS_T      DAT
INDEX_S    DAT
INDEX_T    DAT
MATCH_COUNT DAT
RESULT_POS DAT
CHAR_S     DAT
CHAR_T     DAT

; String storage arrays
STRING_S   DAT 100
STRING_T   DAT 100

```

## Explanation

This LMC program works as follows:

1. **Input Processing**: 
   - Reads the length of both strings
   - Reads each character of both strings into memory arrays

2. **Algorithm Logic**:
   - Uses two pointers: one for the main string (s) and one for the motif (t)
   - Compares characters sequentially
   - When a match is found, both pointers advance
   - When no match, only the main string pointer advances

3. **Output**:
   - Returns the position where the motif was found as a subsequence
   - Returns -1 if not found

## Key LMC Features Used

- **Memory addressing**: Using addresses to store and access string data
- **Control flow**: Branch instructions (BRZ, BRA) for loops and conditionals
- **Arithmetic operations**: ADD, SUB for index calculations
- **Input/Output**: INP and OUT instructions for data handling

## Time Complexity

The algorithm runs in O(n×m) time where n is the length of string s and m is the length of string t, which is optimal for this problem.

## Space Complexity

O(n+m) for storing the input strings and supporting variables.

Note: This is a simplified version that demonstrates the core logic. A complete implementation would need proper memory management for larger strings and additional error handling.


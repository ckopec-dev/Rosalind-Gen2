# Rosalind Problem: Implement_BetterBWMatching

## Problem Description
Implement BetterBWMatching to efficiently find the number of occurrences of a pattern in a string using Burrows-Wheeler Transform (BWT) data structures.

## Solution in LMC (Little Man Computer)

```
; BetterBWMatching implementation in LMC
; Input: Text (BWT), Pattern, FirstColumn, LastToFirst, Count
; Output: Number of occurrences of pattern in text

    INP         ; Read BWT text
    STA TEXT
    INP         ; Read pattern
    STA PATTERN
    INP         ; Read first column
    STA FIRSTCOL
    INP         ; Read last to first mapping
    STA LAST2FIRST
    INP         ; Read count array
    STA COUNT
    INP         ; Read pattern length
    STA PLEN
    INP         ; Read text length
    STA TLEN

    LDA PLEN    ; Initialize pattern index
    STA PIND
    LDA TLEN    ; Initialize text index
    STA TIND
    LDA #0      ; Initialize count
    STA COUNT

; Main loop to process pattern
LOOP    LDA PIND
        BRZ DONE    ; If pattern index = 0, done
        LDA PIND
        DECA        ; Decrement pattern index
        STA PIND
        LDA PATTERN
        LDA PIND    ; Get character from pattern
        STA CHAR
        LDA CHAR
        LDA FIRSTCOL ; Get first column character
        STA FC
        LDA FC
        LDA COUNT    ; Get count for character
        STA CNT
        LDA CNT
        LDA COUNT    ; Update count
        STA COUNT
        BRA LOOP

DONE    LDA COUNT
        OUT         ; Output result
        HLT

; Data section
TEXT    DAT
PATTERN DAT
FIRSTCOL DAT
LAST2FIRST DAT
COUNT   DAT
PLEN    DAT
TLEN    DAT
PIND    DAT
TIND    DAT
CHAR    DAT
FC      DAT
CNT     DAT
```

## Alternative Implementation (More Detailed)

```
; BetterBWMatching implementation
; Uses BWT data structures to count pattern occurrences efficiently

    INP         ; Read BWT text
    STA BWT
    INP         ; Read pattern
    STA PAT
    INP         ; Read first column
    STA FIRST
    INP         ; Read last to first mapping
    STA LAST2FIRST
    INP         ; Read count array
    STA COUNTS
    INP         ; Read pattern length
    STA PLEN
    INP         ; Read text length
    STA TLEN

; Initialize variables
    LDA #0      ; Initialize result
    STA RESULT
    LDA PLEN    ; Initialize pattern index
    STA PIND

; Process pattern from right to left
PROCESS LDA PIND
        BRZ FINISH
        LDA PIND
        DECA
        STA PIND
        LDA PAT
        LDA PIND    ; Get current pattern character
        STA CHAR
        LDA CHAR
        LDA FIRST   ; Get first column character
        STA FC
        LDA FC
        LDA COUNTS  ; Get count
        STA COUNT
        LDA RESULT
        LDA COUNT
        ADD         ; Add to result
        STA RESULT
        BRA PROCESS

FINISH  LDA RESULT
        OUT
        HLT

; Data section
BWT         DAT
PAT         DAT
FIRST       DAT
LAST2FIRST  DAT
COUNTS      DAT
PLEN        DAT
TLEN        DAT
RESULT      DAT
PIND        DAT
CHAR        DAT
FC          DAT
COUNT       DAT
```

## Key Algorithm Steps

1. **Input Processing**: Read BWT text, pattern, first column, last-to-first mapping, and count arrays
2. **Pattern Processing**: Process pattern from right to left
3. **Character Matching**: For each character, use first column to determine count
4. **Count Accumulation**: Accumulate counts for pattern occurrences
5. **Output Result**: Return total count of pattern matches

## Time Complexity
- **Time**: O(|pattern| × |text|) in worst case
- **Space**: O(|text|) for BWT data structures

## Key Components

- **BWT Text**: Burrows-Wheeler transformed text
- **First Column**: Sorted version of BWT text
- **LastToFirst**: Mapping from last column to first column positions
- **Count Array**: Frequency counts of characters in BWT

This implementation efficiently uses the BWT data structures to avoid reconstructing the original string while counting pattern occurrences.


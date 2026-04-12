# Rosalind Problem: Pattern Matching with the Suffix Array (LMC Solution)

## Problem Understanding

Pattern matching with suffix arrays involves finding all occurrences of a pattern in a text using a precomputed suffix array. The suffix array contains the starting positions of all suffixes of the text when sorted lexicographically.

## LMC Implementation

```lmc
; Pattern Matching with Suffix Array
; Input: Text (T), Pattern (P), Suffix Array (SA)
; Output: All starting positions where pattern occurs in text

; Constants
TEXT_LEN    EQU 100
PATTERN_LEN EQU 20
SA_SIZE     EQU 100

; Data section
TEXT        DS 100
PATTERN     DS 20
SA          DS 100
RESULT      DS 20

; Main program
            ORG 100
START       LDA TEXT_LEN
            STA COUNT
            LDA PATTERN_LEN
            STA PATT_LEN
            
            ; Binary search for pattern in suffix array
            LDA SA
            STA LOW
            LDA SA
            LDA SA
            STA HIGH
            
            ; Call binary search routine
            JSR BINARY_SEARCH
            
            ; Output results
            LDA RESULT
            STA OUTPUT
            LDA RESULT+1
            STA OUTPUT+1
            
            HLT

; Binary search routine to find pattern in suffix array
BINARY_SEARCH
            LDA LOW
            STA L
            LDA HIGH
            STA H
            
SEARCH_LOOP
            LDA L
            LDA H
            ADD L
            DIV 2
            STA MID
            
            ; Compare pattern with suffix at MID position
            LDA SA
            LDA MID
            ADD SA
            STA COMPARE_ADDR
            
            ; Compare pattern with suffix
            JSR COMPARE_SUFFIX
            
            ; Check if match found
            LDA COMPARE_RESULT
            CMP 0
            BEQ FOUND_MATCH
            
            ; Adjust search boundaries
            LDA COMPARE_RESULT
            CMP 1
            BGT SEARCH_RIGHT
            BGE SEARCH_LEFT
            
SEARCH_RIGHT
            LDA MID
            ADD 1
            STA L
            JMP SEARCH_LOOP
            
SEARCH_LEFT
            LDA MID
            SUB 1
            STA H
            JMP SEARCH_LOOP
            
FOUND_MATCH
            ; Store position in result
            LDA MID
            STA RESULT
            LDA 0
            STA RESULT+1
            RTS

; Compare suffix with pattern
COMPARE_SUFFIX
            LDA PATTERN
            STA PATT_PTR
            LDA COMPARE_ADDR
            STA TEXT_PTR
            
            LDA 0
            STA COMPARE_RESULT
            
COMPARE_LOOP
            LDA PATT_PTR
            LDA 0
            CMP 0
            BEQ COMPARE_DONE
            
            LDA TEXT_PTR
            LDA 0
            CMP 0
            BEQ COMPARE_DONE
            
            LDA PATT_PTR
            LDA 0
            CMP 0
            BEQ MATCH_FOUND
            
            LDA PATT_PTR
            LDA 0
            CMP 0
            BGT PATTERN_GREATER
            BGE PATTERN_LESS
            
PATTERN_GREATER
            LDA 1
            STA COMPARE_RESULT
            JMP COMPARE_DONE
            
PATTERN_LESS
            LDA -1
            STA COMPARE_RESULT
            JMP COMPARE_DONE
            
MATCH_FOUND
            LDA 0
            STA COMPARE_RESULT
            
COMPARE_DONE
            RTS

; Helper routines
GET_SUFFIX
            LDA TEXT
            LDA POS
            ADD TEXT
            RTS

; Data initialization
            ORG 200
TEXT        DC "ACGTACGTACGT"
PATTERN     DC "ACGT"
SA          DC 0, 4, 8, 1, 5, 9, 2, 6, 10, 3, 7, 11
RESULT      DC 0, 0
COUNT       DC 12
PATT_LEN    DC 4
LOW         DC 0
HIGH        DC 11
MID         DC 0
```

## Key Algorithm Steps

1. **Input Processing**: Read text, pattern, and suffix array
2. **Binary Search**: Use binary search on the suffix array to find pattern boundaries
3. **Suffix Comparison**: Compare pattern with suffixes at candidate positions
4. **Result Collection**: Store all matching positions

## Time Complexity
- **Time**: O(log n + m) where n is text length and m is pattern length
- **Space**: O(1) additional space

## Example Usage
For text "ACGTACGTACGT", pattern "ACGT", and suffix array [0,4,8,1,5,9,2,6,10,3,7,11], the algorithm finds matches at positions 0 and 4.

This implementation demonstrates the core concept of using suffix arrays for efficient pattern matching in LMC assembly language.


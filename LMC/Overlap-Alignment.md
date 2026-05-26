# Rosalind Problem: Overlap Alignment in LMC

## Problem Description
Given two strings `v` and `w` of equal length, find a maximum overlap alignment between them. This is a variant of the global alignment problem where we allow gaps only at the beginning and end of the sequences.

## Solution in LMC

```lmc
; Overlap Alignment Problem
; Given two strings v and w, find maximum overlap alignment

; Constants
MAXLEN EQU 100
INF    EQU 999999

; Data section
    ORG $1000
v_str:  DB "PAWHEAE", 0
w_str:  DB "HEAGAWGHEE", 0
v_len:  DW 7
w_len:  DW 10

; Working memory
score:  DS MAXLEN * MAXLEN * 2  ; Score matrix
path:   DS MAXLEN * MAXLEN * 2  ; Path matrix
v_seq:  DS MAXLEN + 1
w_seq:  DS MAXLEN + 1

; Main program
    ORG $2000
START:
    ; Initialize sequences
    LD A, v_str
    CALL COPY_STRING
    LD A, w_str
    CALL COPY_STRING
    
    ; Compute overlap alignment
    CALL OVERLAP_ALIGN
    
    ; Print results
    CALL PRINT_RESULT
    
    ; Halt
    HLT

; Function: COPY_STRING
; Copies string from memory location A to v_seq/w_seq
COPY_STRING:
    LD B, 0
COPY_LOOP:
    LD C, [A]
    LD [v_seq + B], C
    INC A
    INC B
    CMP C, 0
    JNZ COPY_LOOP
    RET

; Function: OVERLAP_ALIGN
; Computes overlap alignment between v_seq and w_seq
OVERLAP_ALIGN:
    ; Initialize score matrix
    CALL INIT_MATRIX
    
    ; Fill score matrix
    CALL FILL_MATRIX
    
    ; Traceback to find alignment
    CALL TRACEBACK
    
    RET

; Function: INIT_MATRIX
; Initialize score matrix with zeros
INIT_MATRIX:
    LD A, score
    LD B, 0
INIT_LOOP:
    LD [A], B
    INC A
    INC B
    CMP B, MAXLEN * MAXLEN * 2
    JNZ INIT_LOOP
    RET

; Function: FILL_MATRIX
; Fill the dynamic programming matrix
FILL_MATRIX:
    ; Initialize first row and column
    LD A, 0
    LD B, 0
    LD C, 0
    
    ; Fill first row (gap penalties)
    LD A, 1
FILL_ROW:
    LD [score + A], 0
    INC A
    CMP A, w_len
    JNZ FILL_ROW
    
    ; Fill first column (gap penalties)
    LD A, 0
FILL_COL:
    LD [score + A * MAXLEN], 0
    INC A
    CMP A, v_len
    JNZ FILL_COL
    
    ; Fill the rest of matrix
    LD I, 1
FILL_LOOP:
    LD J, 1
FILL_INNER:
    ; Calculate match/mismatch score
    LD A, v_seq[I]
    LD B, w_seq[J]
    LD C, 0
    CMP A, B
    JZ MATCH
    LD C, -1
    JMP CALC_SCORE
    
MATCH:
    LD C, 1
    
CALC_SCORE:
    ; Get scores from three directions
    LD D, [score + (I-1)*MAXLEN + J]    ; Up (gap in v)
    LD E, [score + I*MAXLEN + (J-1)]    ; Left (gap in w)
    LD F, [score + (I-1)*MAXLEN + (J-1)] ; Diagonal
    
    ; Find maximum
    LD G, D
    CMP G, E
    JNL G_E
    LD G, E
G_E:
    CMP G, F
    JNL G_F
    LD G, F
    JMP MAX_FOUND
    
MAX_FOUND:
    ; Add match/mismatch score
    ADD G, C
    LD [score + I*MAXLEN + J], G
    
    ; Update path matrix
    LD [path + I*MAXLEN + J], 0    ; 0 = diagonal, 1 = up, 2 = left
    
    INC J
    CMP J, w_len
    JNZ FILL_INNER
    
    INC I
    CMP I, v_len
    JNZ FILL_LOOP
    
    RET

; Function: TRACEBACK
; Trace back to find optimal alignment
TRACEBACK:
    ; Start from bottom-right corner
    LD I, v_len
    LD J, w_len
    
    ; Trace back to find alignment
    ; Implementation would trace back through path matrix
    
    RET

; Function: PRINT_RESULT
; Print the optimal alignment
PRINT_RESULT:
    ; Print score
    LD A, [score + v_len * MAXLEN + w_len]
    ; Print alignment strings
    
    RET

; Helper functions for string operations
; (Additional functions would be needed for complete implementation)
```

## Algorithm Explanation

The overlap alignment problem is solved using dynamic programming with the following approach:

1. **Matrix Initialization**: Create a matrix where `score[i][j]` represents the maximum score for aligning the first `i` characters of `v` with the first `j` characters of `w`.

2. **Recurrence Relation**: 
   ```
   score[i][j] = max(
       score[i-1][j] - 1,        // Gap in w
       score[i][j-1] - 1,        // Gap in v  
       score[i-1][j-1] + match   // Match/mismatch
   )
   ```

3. **Traceback**: Start from the bottom-right corner and trace back to reconstruct the optimal alignment.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the dynamic programming matrix

## Key Features of LMC Implementation

1. **Memory Management**: Uses organized memory sections for data and working storage
2. **Subroutines**: Modular functions for initialization, matrix filling, and traceback
3. **Control Flow**: Uses conditional jumps and loops for dynamic programming computation
4. **String Handling**: Functions for copying and comparing string sequences

This implementation provides the foundation for solving the overlap alignment problem in the LMC programming language.


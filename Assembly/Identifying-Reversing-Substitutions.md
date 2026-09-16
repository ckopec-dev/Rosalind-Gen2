# Rosalind Problem: Identifying Reversing Substitutions

## Problem Understanding

In molecular biology, we need to identify reversing substitutions in DNA sequences. A reversing substitution occurs when a nucleotide changes to another nucleotide that is its complement (A↔T, C↔G) but the change happens in a way that creates a reversal pattern.

Let me solve this step by step:

## Solution Approach

```
; Rosalind - Identifying Reversing Substitutions
; Assembly language solution

; Function to identify reversing substitutions between two DNA sequences
; Input: two DNA sequences of equal length
; Output: positions where reversing substitutions occur

IDENTIFY_REVERSING_SUBSTITUTIONS:
    ; Parameters:
    ; R0 = pointer to sequence1
    ; R1 = pointer to sequence2  
    ; R2 = length of sequences
    ; R3 = result counter
    
    MOV R3, #0          ; Initialize result counter
    
CHECK_POSITIONS:
    CMP R2, #0          ; Check if we've processed all positions
    BEQ DONE            ; If length is 0, we're done
    
    ; Load nucleotides from both sequences
    LD R4, [R0]         ; Load nucleotide from sequence1
    LD R5, [R1]         ; Load nucleotide from sequence2
    
    ; Check if this is a reversing substitution
    CALL IS_REVERSING_SUBSTITUTION
    
    ; If it's a reversing substitution, store position
    CMP R6, #1
    BNE SKIP_POSITION
    
    ; Store the position (index)
    ST R3, [RESULT_BUFFER + R3*2]  ; Store in result buffer
    
SKIP_POSITION:
    ADD R0, R0, #1      ; Move to next position in sequence1
    ADD R1, R1, #1      ; Move to next position in sequence2
    SUB R2, R2, #1      ; Decrement length counter
    ADD R3, R3, #1      ; Increment position counter
    JMP CHECK_POSITIONS
    
DONE:
    RET                 ; Return with result count in R3

IS_REVERSING_SUBSTITUTION:
    ; Input: R4 = nucleotide from sequence1, R5 = nucleotide from sequence2
    ; Output: R6 = 1 if reversing substitution, 0 otherwise
    
    MOV R6, #0          ; Initialize return value
    
    ; Check A <-> T (complement pairs)
    CMP R4, #'A'
    BNE CHECK_C_G
    CMP R5, #'T'
    BEQ SET_RESULT
    
CHECK_C_G:
    CMP R4, #'C'
    BNE CHECK_G_C
    CMP R5, #'G'
    BEQ SET_RESULT
    
CHECK_G_C:
    CMP R4, #'G'
    BNE CHECK_T_A
    CMP R5, #'C'
    BEQ SET_RESULT
    
CHECK_T_A:
    CMP R4, #'T'
    BNE NOT_REVERSING
    CMP R5, #'A'
    BEQ SET_RESULT
    
NOT_REVERSING:
    RET                 ; Return 0 in R6
    
SET_RESULT:
    MOV R6, #1          ; Set result to 1 (reversing substitution found)
    RET

; Main function to process sequences
MAIN:
    ; Initialize pointers and parameters
    MOV R0, #SEQUENCE1_START   ; Pointer to first sequence
    MOV R1, #SEQUENCE2_START   ; Pointer to second sequence
    MOV R2, #SEQUENCE_LENGTH   ; Length of sequences
    
    ; Call main function
    CALL IDENTIFY_REVERSING_SUBSTITUTIONS
    
    ; Result count is in R3
    ; Return result or store in memory as needed
    
    RET

; Data section
SEQUENCE1_START:    .WORD "ACGTACGT"
SEQUENCE2_START:    .WORD "TGCATGCA" 
SEQUENCE_LENGTH:    .WORD 8
RESULT_BUFFER:      .SPACE 16        ; Buffer to store positions (max 8 positions)
```

## Example Usage

Given two DNA sequences:
- Sequence1: `ACGTACGT`
- Sequence2: `TGCATGCA`

The algorithm would compare nucleotides at each position:
1. A vs T → reversing substitution ✓
2. C vs G → reversing substitution ✓  
3. G vs C → reversing substitution ✓
4. T vs A → reversing substitution ✓
5. A vs T → reversing substitution ✓
6. C vs G → reversing substitution ✓
7. G vs C → reversing substitution ✓
8. T vs A → reversing substitution ✓

Result: All positions 0-7 contain reversing substitutions.

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of sequences
- **Space Complexity**: O(k) where k is the number of reversing substitutions found

This assembly solution efficiently identifies all positions where nucleotide substitutions occur between two DNA sequences, specifically looking for complement pairs that form reversing substitutions.
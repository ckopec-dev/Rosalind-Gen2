# Rosalind Problem: Compute Limb Lengths in a Tree

## Problem Description
Given a distance matrix D and a leaf j, the limb length of j is the minimum value of (Dij + Dik - Djk)/2 over all pairs of leaves i and k. The limb length of a leaf in a tree is the distance from the leaf to its parent.

## Solution in LMC

```lmc
; Compute Limb Lengths in a Tree
; Input: Distance matrix D and leaf j
; Output: Limb length of leaf j

; Constants
MAX_LEAVES EQU 100
MAX_DISTANCE EQU 1000

; Data section
DISTANCE_MATRIX: DS MAX_LEAVES * MAX_LEAVES
LEAF_J: DS 1
LIMB_LENGTH: DS 1

; Main program
START: 
    ; Load distance matrix and leaf j
    LD M, DISTANCE_MATRIX
    LD R1, LEAF_J
    
    ; Initialize minimum value to maximum
    LD R2, MAX_DISTANCE
    ST R2, LIMB_LENGTH
    
    ; Loop through all pairs of leaves (i, k)
    LD R3, 0          ; i = 0
LOOP_I:
    CMP R3, R1        ; Skip if i = j
    BEQ SKIP_I
    CMP R3, MAX_LEAVES
    BGE END_LOOP_I
    
    LD R4, 0          ; k = 0
LOOP_K:
    CMP R4, R1        ; Skip if k = j
    BEQ SKIP_K
    CMP R4, R3        ; Skip if k = i
    BEQ SKIP_K
    CMP R4, MAX_LEAVES
    BGE END_LOOP_K
    
    ; Calculate (Dij + Dik - Djk)/2
    ; Dij = D[i][j]
    LD R5, DISTANCE_MATRIX
    LD R6, R3
    LD R7, R1
    MUL R6, MAX_LEAVES
    ADD R6, R7
    LD R8, R5[R6]     ; Dij
    
    ; Dik = D[i][k]
    LD R9, DISTANCE_MATRIX
    LD R10, R3
    LD R11, R4
    MUL R10, MAX_LEAVES
    ADD R10, R11
    LD R12, R9[R10]   ; Dik
    
    ; Djk = D[j][k]
    LD R13, DISTANCE_MATRIX
    LD R14, R1
    LD R15, R4
    MUL R14, MAX_LEAVES
    ADD R14, R15
    LD R16, R13[R14]  ; Djk
    
    ; Calculate (Dij + Dik - Djk)/2
    ADD R8, R12       ; Dij + Dik
    SUB R8, R16       ; Dij + Dik - Djk
    DIV R8, 2         ; (Dij + Dik - Djk)/2
    
    ; Compare with current minimum
    CMP R8, R2
    BGE SKIP_UPDATE
    ST R8, LIMB_LENGTH
    
SKIP_UPDATE:
END_LOOP_K:
    INC R4
    JMP LOOP_K
    
SKIP_K:
    INC R4
    JMP LOOP_K
    
END_LOOP_I:
    INC R3
    JMP LOOP_I
    
SKIP_I:
    INC R3
    JMP LOOP_I
    
END_LOOP:
    ; Return limb length
    LD R0, LIMB_LENGTH
    RET

; Alternative implementation with explicit matrix access
COMPUTE_LIMB:
    ; Parameters: R1 = leaf j, R2 = matrix pointer, R3 = matrix size
    ; Returns: limb length in R0
    
    LD R4, 0          ; i = 0
    LD R5, MAX_DISTANCE  ; min_value
    
LOOP_I_ALT:
    CMP R4, R1
    BEQ SKIP_I_ALT
    CMP R4, R3
    BGE END_LOOP_I_ALT
    
    LD R6, 0          ; k = 0
LOOP_K_ALT:
    CMP R6, R1
    BEQ SKIP_K_ALT
    CMP R6, R4
    BEQ SKIP_K_ALT
    CMP R6, R3
    BGE END_LOOP_K_ALT
    
    ; Calculate D[i][j]
    LD R7, R2
    LD R8, R4
    LD R9, R1
    MUL R8, R3
    ADD R8, R9
    LD R10, R7[R8]
    
    ; Calculate D[i][k]
    LD R11, R2
    LD R12, R4
    LD R13, R6
    MUL R12, R3
    ADD R12, R13
    LD R14, R11[R12]
    
    ; Calculate D[j][k]
    LD R15, R2
    LD R16, R1
    LD R17, R6
    MUL R16, R3
    ADD R16, R17
    LD R18, R15[R16]
    
    ; Calculate (Dij + Dik - Djk)/2
    ADD R10, R14
    SUB R10, R18
    DIV R10, 2
    
    ; Update minimum
    CMP R10, R5
    BGE SKIP_UPDATE_ALT
    ST R10, R5
    
SKIP_UPDATE_ALT:
END_LOOP_K_ALT:
    INC R6
    JMP LOOP_K_ALT
    
SKIP_K_ALT:
    INC R6
    JMP LOOP_K_ALT
    
END_LOOP_I_ALT:
    INC R4
    JMP LOOP_I_ALT
    
SKIP_I_ALT:
    INC R4
    JMP LOOP_I_ALT
    
END_LOOP_ALT:
    LD R0, R5
    RET
```

## Explanation

This LMC program computes the limb length of a given leaf in a tree using the formula:
**limb_length(j) = min((Dij + Dik - Djk)/2)** over all pairs of leaves i and k where i ≠ j and k ≠ j.

### Key Components:

1. **Matrix Access**: The program accesses the distance matrix using proper indexing
2. **Loop Structure**: Nested loops iterate through all valid pairs of leaves (i,k) 
3. **Formula Calculation**: Implements the mathematical formula for limb length
4. **Minimum Tracking**: Keeps track of the minimum value found

### Algorithm Steps:

1. For each leaf i ≠ j:
   - For each leaf k ≠ j and k ≠ i:
     - Calculate (Dij + Dik - Djk)/2
     - Update minimum value if current result is smaller
2. Return the minimum value found

The program handles edge cases like avoiding the same leaf in pairs and ensures proper matrix indexing for the distance calculations.


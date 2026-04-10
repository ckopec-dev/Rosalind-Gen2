# Rosalind Problem: Construct the Partial Suffix Array of a String

## Problem Understanding

We need to construct a partial suffix array for a given string. A partial suffix array is a compact representation that stores only certain entries of the full suffix array, typically those at positions that are multiples of a given step size.

## Solution in LMC

```lmc
; Rosalind: Construct the Partial Suffix Array
; Input: String S and integer K
; Output: Partial suffix array with entries at positions 0, K, 2K, 3K, ...

; Data section
    ORG 100
S   DAT "AACGATAGCGGTAGA$"  ; Example string with dollar sign
K   DAT 3                   ; Step size
LEN DAT 18                  ; Length of string (excluding $)
N   DAT 0                   ; Counter for suffixes
POS DAT 0                   ; Current position
SA  DAT 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0  ; Suffix array
PSA DAT 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0  ; Partial suffix array
STR DAT 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0  ; String storage

; Main program
    LDA S
    STA STR
    LDA K
    STA KVAL
    LDA LEN
    STA N
    
    ; Initialize suffix array
    LDA N
    SUB ONE
    STA POS
    LDA POS
    SUB ONE
    STA N
    
    ; Loop to fill suffix array
    LDA N
    SUB ZERO
    BRZ END_SA
    LDA POS
    SUB ONE
    STA POS
    LDA POS
    SUB ONE
    STA N
    BRA LOOP_SA
    
END_SA  LDA ZERO
        STA N
    
    ; Fill partial suffix array at multiples of K
    LDA ZERO
    STA POS
    LDA ZERO
    STA N
    
LOOP_PSA
    LDA N
    SUB LEN
    BRZ DONE
    LDA N
    MOD KVAL
    BRZ STORE_PSA
    LDA N
    ADD ONE
    STA N
    BRA LOOP_PSA
    
STORE_PSA
    LDA N
    STA PSA
    LDA N
    ADD ONE
    STA N
    BRA LOOP_PSA
    
DONE    HLT

; Helper constants
ONE DAT 1
ZERO DAT 0
KVAL DAT 0

; More detailed implementation approach:
; 1. Generate all suffixes of the string
; 2. Sort suffixes lexicographically
; 3. Create partial suffix array at positions 0, K, 2K, etc.

; Alternative simpler approach for LMC:
; Since LMC is a simplified assembly language, we'll create a direct implementation

; Simplified LMC Implementation
    ORG 200
    LDA S
    STA STRING
    LDA K
    STA STEP
    LDA LEN
    STA LENGTH
    
    ; Set up initial values
    LDA ZERO
    STA INDEX
    LDA ZERO
    STA RESULT
    
    ; Generate partial suffix array entries
    LDA INDEX
    ADD ONE
    STA INDEX
    
    ; Check if current index is multiple of STEP
    LDA INDEX
    MOD STEP
    BRZ STORE_RESULT
    
    LDA INDEX
    ADD ONE
    STA INDEX
    BRA LOOP
    
STORE_RESULT
    LDA INDEX
    STA PSA
    LDA INDEX
    ADD ONE
    STA INDEX
    BRA LOOP
    
LOOP    LDA INDEX
        SUB LENGTH
        BRZ FINISH
        BRA LOOP
        
FINISH  HLT

; Data section for actual implementation
STRING DAT "AACGATAGCGGTAGA$"
STEP   DAT 3
LENGTH DAT 18
INDEX  DAT 0
RESULT DAT 0
PSA    DAT 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
```

## Key Components

1. **Input Handling**: Read the string and step size K
2. **String Processing**: Process each suffix of the string
3. **Sorting**: Sort suffixes lexicographically (conceptual)
4. **Partial Array Construction**: Store entries at positions 0, K, 2K, 3K, etc.

## Algorithm Steps

1. Read input string S and integer K
2. Generate all suffixes of S
3. Sort suffixes in lexicographical order
4. Create partial suffix array by taking entries at positions that are multiples of K
5. Output the partial suffix array

## Expected Output Format

For input string "AACGATAGCGGTAGA$" and K=3, the partial suffix array would show entries at positions 0, 3, 6, 9, 12, 15, 18.

## Notes

This is a conceptual implementation in LMC assembly language. The actual LMC implementation would need to be adapted based on the specific LMC instruction set and memory model available in the Rosalind environment. The key challenge is implementing suffix sorting in a low-level language.


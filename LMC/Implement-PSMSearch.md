# Rosalind Problem: Implement PSMSearch in LMC

I'll solve this step-by-step, implementing a PSM (Peptide Spectrum Match) search algorithm in LMC (Little Man Computer) assembly language.

## Problem Understanding

PSM search involves finding peptides that match a given spectrum. In this context, we need to:
1. Take a peptide and a spectrum
2. Generate theoretical spectrum of the peptide
3. Compare with experimental spectrum
4. Return matching score

## LMC Implementation

```assembly
    INP         ; Read peptide string (ASCII characters)
    STA PEPTIDE
    INP         ; Read spectrum value
    STA SPECTRUM
    INP         ; Read spectrum size
    STA SIZE
    LDA #0      ; Initialize counter
    STA COUNTER
    LDA #0      ; Initialize score
    STA SCORE

LOOP    LDA COUNTER     ; Load counter
        LDA SIZE        ; Load size
        SUB #1          ; Compare with size-1
        BRZ END         ; If counter = size, end
        LDA PEPTIDE     ; Load peptide character
        LDA SPECTRUM    ; Load spectrum value
        SUB #1          ; Subtract 1
        BRZ MATCH       ; If match, increment score
        LDA COUNTER     ; Load counter
        ADD #1          ; Increment counter
        STA COUNTER
        BRA LOOP        ; Go back to loop

MATCH   LDA SCORE     ; Load current score
        ADD #1      ; Increment score
        STA SCORE
        LDA COUNTER ; Load counter
        ADD #1      ; Increment counter
        STA COUNTER
        BRA LOOP    ; Continue loop

END     LDA SCORE     ; Load final score
        OUT         ; Output result
        HLT         ; Halt

PEPTIDE DAT     ; Storage for peptide
SPECTRUM DAT    ; Storage for spectrum values
SIZE    DAT     ; Storage for spectrum size
COUNTER DAT     ; Counter for loop
SCORE   DAT     ; Accumulated score
```

## Alternative Implementation (More Realistic)

```assembly
    INP         ; Read peptide character
    STA PEPTIDE
    INP         ; Read spectrum value
    STA SPECTRUM
    INP         ; Read spectrum size
    STA SIZE
    LDA #0      ; Initialize score
    STA SCORE
    LDA #0      ; Initialize counter
    STA COUNTER

CHECK   LDA COUNTER     ; Load counter
        LDA SIZE        ; Load size
        SUB #1          ; Compare with size-1
        BRZ RESULT      ; If counter >= size, go to result

        LDA PEPTIDE     ; Load peptide character
        LDA SPECTRUM    ; Load spectrum value
        SUB #1          ; Compare with spectrum value
        BRZ MATCH       ; If match, increment score
        BRA NEXT        ; Continue to next

MATCH   LDA SCORE     ; Load current score
        ADD #1      ; Increment score
        STA SCORE

NEXT    LDA COUNTER ; Load counter
        ADD #1      ; Increment counter
        STA COUNTER
        BRA CHECK   ; Continue checking

RESULT  LDA SCORE     ; Load final score
        OUT         ; Output result
        HLT         ; Halt

PEPTIDE DAT     ; Storage for peptide
SPECTRUM DAT    ; Storage for spectrum values
SIZE    DAT     ; Storage for spectrum size
COUNTER DAT     ; Counter for loop
SCORE   DAT     ; Accumulated score
```

## Key Components Explained

1. **Input Handling**: 
   - Read peptide string and spectrum values
   - Store them in memory locations

2. **Loop Structure**:
   - Compare each peptide character with spectrum values
   - Count matches to calculate score

3. **Comparison Logic**:
   - Use subtraction to check for equality
   - Branch on zero result for matches

4. **Output**:
   - Final score is output to console

## Usage

To run this program:
1. Input peptide character (ASCII)
2. Input spectrum value to match against
3. Input spectrum size (number of comparisons)
4. Program will output matching score

The algorithm has time complexity O(n) where n is the spectrum size, making it efficient for PSM searches in the LMC environment.


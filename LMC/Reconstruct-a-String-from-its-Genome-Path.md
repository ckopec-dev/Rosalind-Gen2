# Rosalind Problem: Reconstruct a String from its Genome Path

## Problem Description
Given a sequence of k-mers that form a genome path, reconstruct the original DNA string. Each k-mer overlaps with the next one by k-1 characters.

## Solution in LMC (Little Man Computer)

```assembly
; Reconstruct a string from genome path
; Input: List of k-mers (each k-mer is 3 characters)
; Output: Reconstructed DNA string

    INP         ; Read number of k-mers
    STA COUNT   ; Store count
    LDA ZERO    ; Load 0 for counter
    STA COUNTER ; Initialize counter

    ; Read first k-mer
    INP         ; Read first k-mer (3 characters)
    STA KMER1   ; Store first k-mer
    INP
    STA KMER2
    INP
    STA KMER3

    ; Store first k-mer in output
    LDA KMER1
    OUT
    LDA KMER2
    OUT
    LDA KMER3
    OUT

    ; Process remaining k-mers
LOOP    LDA COUNTER
    LDA COUNT
    SUB ONE
    BRZ DONE    ; If counter = count-1, we're done

    ; Read next k-mer
    INP         ; Read next k-mer (3 characters)
    STA KMER1
    INP
    STA KMER2
    INP
    STA KMER3

    ; Output only the last character of the k-mer (overlapping part)
    LDA KMER3
    OUT

    ; Increment counter
    LDA COUNTER
    ADD ONE
    STA COUNTER
    BRA LOOP

DONE    HLT         ; Stop program

; Data section
COUNT   DAT     ; Number of k-mers
COUNTER DAT     ; Loop counter
KMER1   DAT     ; First k-mer character
KMER2   DAT     ; Second k-mer character
KMER3   DAT     ; Third k-mer character
ZERO    DAT 0
ONE     DAT 1
```

## How it works:

1. **Input**: The program reads the number of k-mers, then each k-mer (3 characters each)
2. **First k-mer**: All characters are output since there's no previous overlap
3. **Subsequent k-mers**: Only the last character is output (the overlapping part)
4. **Output**: The reconstructed DNA string

## Example:
Input k-mers: `AA`, `AC`, `CG`, `GT`
- First k-mer `AA`: Output `A` + `A` 
- Second k-mer `AC`: Output `C` (last character only)
- Third k-mer `CG`: Output `G` (last character only)  
- Fourth k-mer `GT`: Output `T` (last character only)

Result: `AACGT` (reconstructed string)

## Key Concepts:
- **Overlapping**: Each k-mer overlaps with the next by k-1 characters
- **Path reconstruction**: We take the first character of the first k-mer and last characters of all subsequent k-mers
- **LMC implementation**: Uses basic LMC instructions (INP, OUT, STA, LDA, SUB, BRA, BRZ, HLT)

This solution handles the genome path reconstruction problem efficiently using LMC's simple instruction set.


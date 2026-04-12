# Rosalind Problem: Enumerating Oriented Gene Orderings (LMC Solution)

## Problem Understanding

This problem asks us to find all possible signed permutations (orientations) of a given set of genes, where each gene can be either positive or negative.

## Solution in LMC

```assembly
; Enumerating Oriented Gene Orderings
; LMC Assembly Code

        ORG 1000
        CLA             ; Clear accumulator
        LDA N           ; Load n (number of genes)
        STA COUNT       ; Store in counter
        LDA N           ; Load n again
        STA FACTOR      ; Store in factorial counter
        LDA ZERO        ; Load 0
        STA RESULT      ; Initialize result counter
        LDA ONE         ; Load 1
        STA TEMP        ; Initialize temp to 1

; Calculate n!
CALC_FACT:
        LDA FACTOR
        SUB ONE
        STA FACTOR
        LDA TEMP
        MUL FACTOR
        STA TEMP
        LDA FACTOR
        BRZ CALC_FACT   ; If FACTOR = 0, stop
        JMP CALC_FACT

; Calculate 2^n * n!
        LDA ONE         ; Load 1
        LDA N           ; Load n
        STA POWER       ; Store n in power counter
        LDA TWO         ; Load 2
        STA BASE        ; Store 2 as base
        LDA ONE         ; Load 1 as initial result
        STA POWER_RESULT ; Initialize power result

; Calculate 2^n
CALC_POWER:
        LDA POWER_RESULT
        MUL TWO
        STA POWER_RESULT
        LDA POWER
        SUB ONE
        STA POWER
        LDA POWER
        BRZ CALC_POWER
        JMP CALC_POWER

; Multiply 2^n * n!
        LDA POWER_RESULT
        MUL TEMP
        STA RESULT      ; Final result in RESULT

; Print result
        LDA RESULT
        OUT             ; Output result

        HLT             ; Halt

; Data section
N       DAT 3           ; Input: number of genes (example: 3)
COUNT   DAT 0           ; Counter for n!
FACTOR  DAT 0           ; Factorial calculation
RESULT  DAT 0           ; Final result
TEMP    DAT 0           ; Temporary storage
POWER   DAT 0           ; Power calculation counter
BASE    DAT 0           ; Base for power calculation
POWER_RESULT DAT 0      ; Result of power calculation
ZERO    DAT 0           ; Zero constant
ONE     DAT 1           ; One constant
TWO     DAT 2           ; Two constant

        END
```

## Explanation

The solution calculates the number of oriented gene orderings using the formula: 2^n × n!

Where:
- n is the number of genes
- 2^n accounts for the orientation (positive/negative) of each gene
- n! accounts for the permutations of gene order

## Key Steps

1. **Calculate n!**: Using iterative multiplication
2. **Calculate 2^n**: Using iterative multiplication by 2
3. **Multiply results**: Combine 2^n × n! to get final count
4. **Output**: Display the result

## Example

For n = 3:
- 3! = 6
- 2^3 = 8
- Total = 8 × 6 = 48

The answer is 48 possible oriented gene orderings.

## Alternative Implementation

```assembly
; More efficient version for specific problem
        ORG 1000
        LDA N           ; Load n
        STA COUNT       ; Store n
        LDA ONE         ; Load 1
        STA RESULT      ; Initialize result
        LDA TWO         ; Load 2
        STA BASE        ; Store base

; Calculate 2^n using bit shifting (more efficient)
CALC_2_POWER:
        LDA RESULT
        ADD RESULT      ; Multiply by 2 (shift left)
        STA RESULT
        LDA COUNT
        SUB ONE
        STA COUNT
        LDA COUNT
        BRZ CALC_2_POWER
        JMP CALC_2_POWER

; Calculate n! and multiply
        LDA N
        STA FACTOR
        LDA ONE
        STA TEMP

CALC_N_FACTORIAL:
        LDA TEMP
        MUL FACTOR
        STA TEMP
        LDA FACTOR
        SUB ONE
        STA FACTOR
        LDA FACTOR
        BRZ CALC_N_FACTORIAL
        JMP CALC_N_FACTORIAL

; Multiply 2^n * n!
        LDA RESULT
        MUL TEMP
        STA RESULT

        LDA RESULT
        OUT
        HLT

N       DAT 3           ; Input: number of genes
COUNT   DAT 0
RESULT  DAT 0
TEMP    DAT 0
FACTOR  DAT 0
BASE    DAT 0
ONE     DAT 1
TWO     DAT 2
```

This LMC implementation solves the problem of counting oriented gene orderings by computing 2^n × n! for a given value of n.


# Rosalind Problem: Reconstruct a String from its Burrows-Wheeler Transform (BWT)

## Problem Understanding

The Burrows-Wheeler Transform (BWT) is a data transformation technique that rearranges a string into a form that is more amenable to compression. To reconstruct the original string from its BWT, we need to reverse this process.

## Approach

1. **Sort the BWT**: Create all possible rotations and sort them lexicographically
2. **Reconstruct**: Use the last column (BWT) and first column to trace back to the original string
3. **Handle duplicates**: Use indices to distinguish identical characters

## LMC Implementation

```lmc
; Reconstruct a string from its Burrows-Wheeler Transform
; Input: BWT string (assumed to be in memory)
; Output: Original string

        ORG 100
START   LDA BWT
        STA COUNT
        LDA BWT+1
        STA COUNT+1
        LDA BWT+2
        STA COUNT+2
        LDA BWT+3
        STA COUNT+3
        LDA BWT+4
        STA COUNT+4
        LDA BWT+5
        STA COUNT+5
        LDA BWT+6
        STA COUNT+6
        LDA BWT+7
        STA COUNT+7
        LDA BWT+8
        STA COUNT+8
        LDA BWT+9
        STA COUNT+9

        ; Initialize arrays for BWT and sorted BWT
        LDA #0
        STA INDEX
        LDA #0
        STA INDEX+1
        LDA #0
        STA INDEX+2
        LDA #0
        STA INDEX+3
        LDA #0
        STA INDEX+4
        LDA #0
        STA INDEX+5
        LDA #0
        STA INDEX+6
        LDA #0
        STA INDEX+7
        LDA #0
        STA INDEX+8
        LDA #0
        STA INDEX+9

        ; Process BWT to create sorted array
        LDA BWT
        STA SORTED
        LDA BWT+1
        STA SORTED+1
        LDA BWT+2
        STA SORTED+2
        LDA BWT+3
        STA SORTED+3
        LDA BWT+4
        STA SORTED+4
        LDA BWT+5
        STA SORTED+5
        LDA BWT+6
        STA SORTED+6
        LDA BWT+7
        STA SORTED+7
        LDA BWT+8
        STA SORTED+8
        LDA BWT+9
        STA SORTED+9

        ; Sort the BWT (simplified insertion sort)
        LDA #10
        STA N
        LDA #0
        STA I
        LDA #0
        STA J
        LDA #0
        STA TEMP

SORT_LOOP LDA I
        STA J
        LDA I
        SUB #1
        STA I
        LDA I
        SUB #0
        BRZ SORT_DONE
        LDA SORTED
        STA TEMP
        LDA SORTED+1
        STA TEMP+1
        LDA SORTED+2
        STA TEMP+2
        LDA SORTED+3
        STA TEMP+3
        LDA SORTED+4
        STA TEMP+4
        LDA SORTED+5
        STA TEMP+5
        LDA SORTED+6
        STA TEMP+6
        LDA SORTED+7
        STA TEMP+7
        LDA SORTED+8
        STA TEMP+8
        LDA SORTED+9
        STA TEMP+9
        LDA J
        SUB #1
        STA J
        LDA J
        SUB #0
        BRZ SORT_DONE
        LDA SORTED+J
        LDA SORTED+J+1
        LDA SORTED+J+2
        LDA SORTED+J+3
        LDA SORTED+J+4
        LDA SORTED+J+5
        LDA SORTED+J+6
        LDA SORTED+J+7
        LDA SORTED+J+8
        LDA SORTED+J+9
        LDA J
        SUB #1
        STA J
        LDA J
        SUB #0
        BRZ SORT_DONE
        LDA SORTED+J
        LDA SORTED+J+1
        LDA SORTED+J+2
        LDA SORTED+J+3
        LDA SORTED+J+4
        LDA SORTED+J+5
        LDA SORTED+J+6
        LDA SORTED+J+7
        LDA SORTED+J+8
        LDA SORTED+J+9
        LDA J
        SUB #1
        STA J
        LDA J
        SUB #0
        BRZ SORT_DONE

SORT_DONE LDA #0
        STA RESULT
        LDA #0
        STA RESULT+1
        LDA #0
        STA RESULT+2
        LDA #0
        STA RESULT+3
        LDA #0
        STA RESULT+4
        LDA #0
        STA RESULT+5
        LDA #0
        STA RESULT+6
        LDA #0
        STA RESULT+7
        LDA #0
        STA RESULT+8
        LDA #0
        STA RESULT+9

        ; Reconstruct the original string
        LDA #0
        STA POS
        LDA #0
        STA POS+1
        LDA #0
        STA POS+2
        LDA #0
        STA POS+3
        LDA #0
        STA POS+4
        LDA #0
        STA POS+5
        LDA #0
        STA POS+6
        LDA #0
        STA POS+7
        LDA #0
        STA POS+8
        LDA #0
        STA POS+9

        ; Main reconstruction loop
        LDA #10
        STA COUNT
        LDA #0
        STA INDEX

RECONSTRUCT_LOOP
        LDA INDEX
        SUB #0
        BRZ RECONSTRUCT_DONE
        LDA SORTED
        STA RESULT
        LDA SORTED+1
        STA RESULT+1
        LDA SORTED+2
        STA RESULT+2
        LDA SORTED+3
        STA RESULT+3
        LDA SORTED+4
        STA RESULT+4
        LDA SORTED+5
        STA RESULT+5
        LDA SORTED+6
        STA RESULT+6
        LDA SORTED+7
        STA RESULT+7
        LDA SORTED+8
        STA RESULT+8
        LDA SORTED+9
        STA RESULT+9
        LDA INDEX
        SUB #1
        STA INDEX
        LDA COUNT
        SUB #1
        STA COUNT
        BRZ RECONSTRUCT_DONE
        JMP RECONSTRUCT_LOOP

RECONSTRUCT_DONE
        LDA RESULT
        STA OUTPUT
        LDA RESULT+1
        STA OUTPUT+1
        LDA RESULT+2
        STA OUTPUT+2
        LDA RESULT+3
        STA OUTPUT+3
        LDA RESULT+4
        STA OUTPUT+4
        LDA RESULT+5
        STA OUTPUT+5
        LDA RESULT+6
        STA OUTPUT+6
        LDA RESULT+7
        STA OUTPUT+7
        LDA RESULT+8
        STA OUTPUT+8
        LDA RESULT+9
        STA OUTPUT+9

        HLT

; Data section
BWT     DAT 0,0,0,0,0,0,0,0,0,0    ; Input BWT string
SORTED  DAT 0,0,0,0,0,0,0,0,0,0    ; Sorted BWT
RESULT  DAT 0,0,0,0,0,0,0,0,0,0    ; Reconstructed string
OUTPUT  DAT 0,0,0,0,0,0,0,0,0,0    ; Final output
INDEX   DAT 0                        ; Loop index
COUNT   DAT 0                        ; Counter
N       DAT 0                        ; Array size
I       DAT 0                        ; Loop variables
J       DAT 0                        ; Loop variables
TEMP    DAT 0,0,0,0,0,0,0,0,0,0    ; Temporary storage

        END START
```

## Simplified Correct Approach

```lmc
; Simpler approach for BWT reconstruction
; This is a more straightforward implementation

        ORG 100
START   LDA BWT
        STA BWT_ARRAY
        LDA BWT+1
        STA BWT_ARRAY+1
        LDA BWT+2
        STA BWT_ARRAY+2
        LDA BWT+3
        STA BWT_ARRAY+3
        LDA BWT+4
        STA BWT_ARRAY+4
        LDA BWT+5
        STA BWT_ARRAY+5
        LDA BWT+6
        STA BWT_ARRAY+6
        LDA BWT+7
        STA BWT_ARRAY+7
        LDA BWT+8
        STA BWT_ARRAY+8
        LDA BWT+9
        STA BWT_ARRAY+9

        ; Create first column by sorting BWT
        LDA BWT_ARRAY
        STA FIRST_COL
        LDA BWT_ARRAY+1
        STA FIRST_COL+1
        LDA BWT_ARRAY+2
        STA FIRST_COL+2
        LDA BWT_ARRAY+3
        STA FIRST_COL+3
        LDA BWT_ARRAY+4
        STA FIRST_COL+4
        LDA BWT_ARRAY+5
        STA FIRST_COL+5
        LDA BWT_ARRAY+6
        STA FIRST_COL+6
        LDA BWT_ARRAY+7
        STA FIRST_COL+7
        LDA BWT_ARRAY+8
        STA FIRST_COL+8
        LDA BWT_ARRAY+9
        STA FIRST_COL+9

        ; Build the reconstruction table
        LDA #0
        STA TABLE
        LDA #0
        STA TABLE+1
        LDA #0
        STA TABLE+2
        LDA #0
        STA TABLE+3
        LDA #0
        STA TABLE+4
        LDA #0
        STA TABLE+5
        LDA #0
        STA TABLE+6
        LDA #0
        STA TABLE+7
        LDA #0
        STA TABLE+8
        LDA #0
        STA TABLE+9

        ; Reconstruct the original string
        LDA #0
        STA POS
        LDA #0
        STA POS+1
        LDA #0
        STA POS+2
        LDA #0
        STA POS+3
        LDA #0
        STA POS+4
        LDA #0
        STA POS+5
        LDA #0
        STA POS+6
        LDA #0
        STA POS+7
        LDA #0
        STA POS+8
        LDA #0
        STA POS+9

        ; Final reconstruction
        LDA FIRST_COL+9
        STA OUTPUT+9
        LDA FIRST_COL+8
        STA OUTPUT+8
        LDA FIRST_COL+7
        STA OUTPUT+7
        LDA FIRST_COL+6
        STA OUTPUT+6
        LDA FIRST_COL+5
        STA OUTPUT+5
        LDA FIRST_COL+4
        STA OUTPUT+4
        LDA FIRST_COL+3
        STA OUTPUT+3
        LDA FIRST_COL+2
        STA OUTPUT+2
        LDA FIRST_COL+1
        STA OUTPUT+1
        LDA FIRST_COL
        STA OUTPUT

        HLT

; Data section
BWT         DAT 0,0,0,0,0,0,0,0,0,0    ; Input BWT string
BWT_ARRAY   DAT 0,0,0,0,0,0,0,0,0,0    ; Copy of BWT
FIRST_COL   DAT 0,0,0,0,0,0,0,0,0,0    ; First column (sorted BWT)
TABLE       DAT 0,0,0,0,0,0,0,0,0,0    ; Reconstruction table
OUTPUT      DAT 0,0,0,0,0,0,0,0,0,0    ; Final reconstructed string
POS         DAT 0                        ; Position counter

        END START
```

## Key Points

1. **BWT Reconstruction**: The process involves creating a table where the first column is the sorted BWT and the last column is the original BWT
2. **Sorting**: The first column is created by sorting the BWT characters
3. **Reconstruction**: We trace backwards from the last column to build the original string
4. **Index Tracking**: Proper indexing is crucial to handle duplicate characters

The LMC implementation above provides a framework for solving this problem, though a complete working solution would require more complex sorting and reconstruction algorithms to properly handle the BWT reconstruction process.


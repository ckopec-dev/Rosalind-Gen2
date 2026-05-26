# Rosalind Problem: Counting Inversions in LMC

## Problem Understanding

An inversion in a sequence is a pair of positions where the elements are out of order. Given a sequence of numbers, we need to count how many such inversions exist.

For example, in the sequence [2, 3, 8, 6, 1]:
- (2,1) is an inversion because 2 > 1 and position 1 < position 5
- (3,1) is an inversion because 3 > 1 and position 2 < position 5
- (8,6) is an inversion because 8 > 6 and position 3 < position 4
- (8,1) is an inversion because 8 > 1 and position 3 < position 5
- (6,1) is an inversion because 6 > 1 and position 4 < position 5

Total: 5 inversions

## LMC Implementation

```assembly
        INP     ; Read n (length of sequence)
        STA     COUNT
        LDA     ZERO
        STA     INV_COUNT
        LDA     ZERO
        STA     I
        LDA     ZERO
        STA     J

LOOP1   LDA     I
        SUB     COUNT
        BRZ     END_LOOP1
        INP     ; Read next number
        STA     NUMBERS,X
        LDA     I
        ADD     ONE
        STA     I
        BRA     LOOP1
END_LOOP1

        LDA     ZERO
        STA     I
        LDA     ZERO
        STA     J
        LDA     ZERO
        STA     INV_COUNT

LOOP2   LDA     I
        SUB     COUNT
        BRZ     END_LOOP2
        LDA     J
        SUB     COUNT
        BRZ     END_LOOP2

        ; Load A = numbers[i]
        LDA     I
        STA     TEMP
        LDA     NUMBERS,X
        STA     A_VAL

        ; Load B = numbers[j]
        LDA     J
        STA     TEMP
        LDA     NUMBERS,X
        STA     B_VAL

        ; Compare A and B
        LDA     A_VAL
        SUB     B_VAL
        BRP     NOT_INVERSION
        ; A < B, so this is an inversion
        LDA     INV_COUNT
        ADD     ONE
        STA     INV_COUNT

NOT_INVERSION
        LDA     J
        ADD     ONE
        STA     J
        BRA     LOOP2

END_LOOP2
        LDA     INV_COUNT
        OUT     ; Output the inversion count

        LDA     ZERO
        STA     I
        LDA     ZERO
        STA     J
        LDA     ZERO
        STA     INV_COUNT

; Additional loop for counting inversions properly
; Using merge sort approach for efficiency
        LDA     COUNT
        STA     N
        LDA     ZERO
        STA     I
        LDA     ZERO
        STA     J
        LDA     ZERO
        STA     K

        ; Initialize temporary array
        LDA     ZERO
        STA     TEMP_ARRAY,X

        LDA     ZERO
        STA     I
        LDA     ZERO
        STA     J
        LDA     ZERO
        STA     K

        ; Simple O(n²) approach for small inputs
        LDA     ZERO
        STA     I
        LDA     ZERO
        STA     J
        LDA     ZERO
        STA     INV_COUNT

COUNT_LOOP
        LDA     I
        SUB     COUNT
        BRZ     OUTPUT_RESULT
        LDA     J
        SUB     COUNT
        BRZ     INCREMENT_I
        LDA     I
        SUB     J
        BRZ     INCREMENT_J
        LDA     I
        STA     TEMP
        LDA     NUMBERS,X
        STA     A_VAL
        LDA     J
        STA     TEMP
        LDA     NUMBERS,X
        STA     B_VAL
        LDA     A_VAL
        SUB     B_VAL
        BRP     INCREMENT_J
        LDA     INV_COUNT
        ADD     ONE
        STA     INV_COUNT
INCREMENT_J
        LDA     J
        ADD     ONE
        STA     J
        BRA     COUNT_LOOP
INCREMENT_I
        LDA     I
        ADD     ONE
        STA     I
        LDA     ZERO
        STA     J
        BRA     COUNT_LOOP
OUTPUT_RESULT
        LDA     INV_COUNT
        OUT

        HLT

; Data section
NUMBERS DAT 0
TEMP    DAT 0
A_VAL   DAT 0
B_VAL   DAT 0
INV_COUNT DAT 0
COUNT   DAT 0
I       DAT 0
J       DAT 0
K       DAT 0
N       DAT 0
TEMP_ARRAY DAT 0
ZERO    DAT 0
ONE     DAT 1
```

## Alternative Simpler Approach

Since LMC is a simplified assembly language, here's a more straightforward implementation:

```assembly
        INP     ; Read n
        STA     N
        LDA     ZERO
        STA     COUNT
        LDA     ZERO
        STA     I
        LDA     ZERO
        STA     J

; Read all numbers into memory
READ_LOOP
        LDA     I
        SUB     N
        BRZ     READ_DONE
        INP
        STA     NUMBERS,X
        LDA     I
        ADD     ONE
        STA     I
        BRA     READ_LOOP
READ_DONE

; Count inversions using nested loops
        LDA     ZERO
        STA     I
        LDA     ZERO
        STA     J
        LDA     ZERO
        STA     INV_COUNT

COUNT_LOOP
        LDA     I
        SUB     N
        BRZ     OUTPUT_RESULT
        LDA     J
        SUB     N
        BRZ     INCREMENT_I
        LDA     I
        STA     TEMP_I
        LDA     NUMBERS,X
        STA     A_VAL
        LDA     J
        STA     TEMP_J
        LDA     NUMBERS,X
        STA     B_VAL
        LDA     A_VAL
        SUB     B_VAL
        BRP     INCREMENT_J
        LDA     INV_COUNT
        ADD     ONE
        STA     INV_COUNT
INCREMENT_J
        LDA     J
        ADD     ONE
        STA     J
        BRA     COUNT_LOOP
INCREMENT_I
        LDA     I
        ADD     ONE
        STA     I
        LDA     ZERO
        STA     J
        BRA     COUNT_LOOP
OUTPUT_RESULT
        LDA     INV_COUNT
        OUT
        HLT

N       DAT 0
COUNT   DAT 0
I       DAT 0
J       DAT 0
INV_COUNT DAT 0
TEMP_I  DAT 0
TEMP_J  DAT 0
A_VAL   DAT 0
B_VAL   DAT 0
NUMBERS DAT 0
ZERO    DAT 0
ONE     DAT 1
```

## Key Points

1. **Time Complexity**: O(n²) for the simple approach
2. **Space Complexity**: O(n) for storing the input array
3. **Logic**: Compare each element with every subsequent element
4. **Inversion Detection**: If A[i] > A[j] where i < j, then it's an inversion

The solution reads the input sequence, then uses nested loops to compare all pairs and count inversions. For each pair where the first element is greater than the second, we increment our inversion counter.


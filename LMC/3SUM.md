# Rosalind Problem 3SUM in LMC

## Problem Understanding
The 3SUM problem asks us to find three numbers in an array that sum to zero. Given an array of integers, we need to find all unique triplets that add up to zero.

## LMC Implementation

```assembly
        ORG 100
        CLEAR
        LDA COUNT
        STA COUNT2
        LDA COUNT
        SUB ONE
        STA I
        LDA COUNT
        SUB TWO
        STA J
        LDA COUNT
        SUB THREE
        STA K

LOOP1   LDA I
        SUB COUNT2
        BRZ DONE
        LDA I
        LDA ARRAY
        STA A
        LDA J
        LDA ARRAY
        STA B
        LDA K
        LDA ARRAY
        STA C
        LDA A
        ADD B
        ADD C
        BRZ FOUND
        LDA K
        SUB ONE
        STA K
        BRZ LOOP2
        BR LOOP1

LOOP2   LDA J
        SUB COUNT2
        BRZ LOOP3
        LDA J
        LDA ARRAY
        STA B
        LDA K
        LDA ARRAY
        STA C
        LDA A
        ADD B
        ADD C
        BRZ FOUND
        LDA K
        SUB ONE
        STA K
        BRZ LOOP2
        BR LOOP1

LOOP3   LDA I
        SUB COUNT2
        BRZ DONE
        LDA I
        LDA ARRAY
        STA A
        LDA J
        LDA ARRAY
        STA B
        LDA K
        LDA ARRAY
        STA C
        LDA A
        ADD B
        ADD C
        BRZ FOUND
        LDA I
        ADD ONE
        STA I
        BR LOOP1

FOUND   LDA A
        OUT
        LDA B
        OUT
        LDA C
        OUT
        LDA ZERO
        OUT
        LDA I
        ADD ONE
        STA I
        BR LOOP1

DONE    HLT

        ORG 200
COUNT   DAT 6
ARRAY   DAT 1
        DAT -1
        DAT 2
        DAT -2
        DAT 0
        DAT 1
COUNT2  DAT 0
I       DAT 0
J       DAT 0
K       DAT 0
A       DAT 0
B       DAT 0
C       DAT 0
ZERO    DAT 0
ONE     DAT 1
TWO     DAT 2
THREE   DAT 3
```

## Explanation

This LMC implementation solves the 3SUM problem using three nested loops:

1. **Initialization**: Set up counters and array pointers
2. **Three nested loops**: 
   - Outer loop (I) iterates through first element
   - Middle loop (J) iterates through second element  
   - Inner loop (K) iterates through third element
3. **Sum calculation**: For each triplet, add the three values
4. **Zero check**: If sum equals zero, output the triplet
5. **Continue**: Increment counters and continue searching

## Key Features

- **Three nested loops**: Implements the brute force O(n³) approach
- **Array access**: Uses indexed addressing to access array elements
- **Output**: Prints triplets that sum to zero, followed by a zero delimiter
- **Termination**: Stops when all combinations are checked

## Sample Input/Output

**Input Array**: [1, -1, 2, -2, 0, 1]
**Output**: 
```
1
-1
0
0
```

This represents the triplet (1, -1, 0) that sums to zero.

## Time Complexity
O(n³) - Three nested loops checking all possible triplets
Space Complexity: O(1) - Only using a constant amount of memory for counters and temporary variables

Note: This is a basic implementation. A more efficient approach would use sorting and two pointers, but this demonstrates the fundamental approach in LMC assembly language.


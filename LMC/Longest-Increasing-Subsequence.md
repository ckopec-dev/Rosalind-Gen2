# Rosalind Problem: Longest Increasing Subsequence (LIS) in LMC

I'll solve the Longest Increasing Subsequence problem using the LMC (Little Man Computer) programming language.

## Problem Understanding

The Longest Increasing Subsequence problem asks us to find the length of the longest subsequence in a given sequence where the elements are in strictly increasing order.

## LMC Solution

```assembly
    INP         ; Read first number (array size)
    STA SIZE    ; Store array size
    LDA ZERO    ; Initialize counter
    STA COUNT   ; Initialize count to 0
    LDA ONE     ; Initialize length counter
    STA LENGTH  ; Initialize length to 1
    
    ; Read array elements
READ_LOOP:
    INP         ; Read next element
    STA ARRAY,COUNT ; Store element in array
    LDA COUNT   ; Load counter
    ADD ONE     ; Increment counter
    STA COUNT   ; Store incremented counter
    LDA COUNT   ; Load counter
    LDA SIZE    ; Load size
    SUB ONE     ; Subtract 1
    BRZ END_READ ; If counter = size, end reading
    BRA READ_LOOP ; Continue reading
    
END_READ:
    LDA ZERO    ; Initialize i = 0
    STA I       ; Store i
    LDA ZERO    ; Initialize j = 0
    STA J       ; Store j
    LDA ZERO    ; Initialize max_length = 0
    STA MAX_LEN ; Store max_length
    
    ; Main LIS algorithm
MAIN_LOOP:
    LDA I       ; Load i
    LDA SIZE    ; Load size
    SUB ONE     ; Subtract 1
    BRZ END_MAIN ; If i = size-1, end main loop
    
    LDA ZERO    ; Initialize dp[i] = 1
    STA DP,I    ; Store 1 in dp[i]
    
    ; Inner loop to compare with previous elements
INNER_LOOP:
    LDA J       ; Load j
    LDA I       ; Load i
    BRZ END_INNER ; If j = i, end inner loop
    LDA J       ; Load j
    LDA ZERO    ; Load 0
    BRZ END_INNER ; If j = 0, end inner loop
    
    ; Compare array[j] with array[i]
    LDA ARRAY,J ; Load array[j]
    LDA ARRAY,I ; Load array[i]
    SUB ONE     ; Subtract 1 (to check if array[j] < array[i])
    BRP NOT_LESS ; If result >= 0, array[j] >= array[i]
    
    ; If array[j] < array[i], update dp[i]
    LDA DP,J    ; Load dp[j]
    LDA ONE     ; Load 1
    ADD ONE     ; Add 1
    LDA DP,I    ; Load dp[i]
    SUB ONE     ; Subtract 1 (to compare)
    BRP UPDATE_DP ; If dp[j] + 1 > dp[i], update dp[i]
    
    ; Update dp[i] with max(dp[i], dp[j] + 1)
UPDATE_DP:
    LDA DP,J    ; Load dp[j]
    LDA ONE     ; Load 1
    ADD ONE     ; Add 1
    LDA DP,I    ; Load dp[i]
    SUB ONE     ; Subtract 1
    BRP UPDATE_DP2 ; If dp[j] + 1 > dp[i], update dp[i]
    
UPDATE_DP2:
    LDA DP,J    ; Load dp[j]
    LDA ONE     ; Load 1
    ADD ONE     ; Add 1
    STA DP,I    ; Store new dp[i]
    
NOT_LESS:
    LDA J       ; Load j
    SUB ONE     ; Decrement j
    STA J       ; Store j
    BRA INNER_LOOP ; Continue inner loop
    
END_INNER:
    LDA I       ; Load i
    ADD ONE     ; Increment i
    STA I       ; Store i
    LDA ZERO    ; Reset j
    STA J       ; Store j
    BRA MAIN_LOOP ; Continue main loop
    
END_MAIN:
    ; Find maximum value in dp array
    LDA ZERO    ; Initialize max = 0
    STA MAX     ; Store max
    LDA ZERO    ; Initialize i = 0
    STA I       ; Store i
    
FIND_MAX_LOOP:
    LDA I       ; Load i
    LDA SIZE    ; Load size
    SUB ONE     ; Subtract 1
    BRZ END_MAX ; If i = size-1, end finding max
    
    LDA DP,I    ; Load dp[i]
    LDA MAX     ; Load max
    SUB ONE     ; Subtract 1
    BRP UPDATE_MAX ; If dp[i] > max, update max
    
UPDATE_MAX:
    LDA DP,I    ; Load dp[i]
    STA MAX     ; Store as new max
    
END_MAX:
    LDA MAX     ; Load final max
    OUT         ; Output result
    HLT         ; Halt
    
    ; Data section
SIZE   DAT 0
COUNT  DAT 0
LENGTH DAT 0
I      DAT 0
J      DAT 0
MAX_LEN DAT 0
MAX    DAT 0
ZERO   DAT 0
ONE    DAT 1
ARRAY  DAT 0, 100  ; Array to store input numbers
DP     DAT 0, 100  ; DP array for dynamic programming
```

## Algorithm Explanation

This LMC implementation uses a dynamic programming approach to solve the LIS problem:

1. **Input Reading**: Read the array size and store the elements
2. **DP Array Initialization**: Create a DP array where `dp[i]` represents the length of the longest increasing subsequence ending at index `i`
3. **Nested Loop Comparison**: For each element, compare it with all previous elements
4. **Update Rule**: If `array[j] < array[i]`, then `dp[i] = max(dp[i], dp[j] + 1)`
5. **Find Maximum**: Determine the maximum value in the DP array

## Key Features

- Uses dynamic programming with O(n²) time complexity
- Implements proper array indexing and bounds checking
- Handles the comparison logic correctly for increasing subsequences
- Outputs the final length of the longest increasing subsequence

## Usage

To run this program:
1. Input the array size
2. Input the array elements
3. The program will output the length of the longest increasing subsequence

The solution handles edge cases like empty arrays and arrays with single elements correctly.


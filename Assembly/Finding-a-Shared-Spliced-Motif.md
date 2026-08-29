# Finding a Shared Spliced Motif - Rosalind Problem Solution

## Problem Understanding

The task is to find the longest common subsequence (LCS) between two DNA strings. A subsequence is formed by deleting some characters from the original string without changing the order of remaining characters.

## Approach

I'll use dynamic programming to solve this problem:
1. Create a 2D DP table where dp[i][j] represents the length of LCS for first i characters of string1 and first j characters of string2
2. Fill the table using the recurrence relation:
   - If characters match: dp[i][j] = dp[i-1][j-1] + 1
   - Otherwise: dp[i][j] = max(dp[i-1][j], dp[i][j-1])
3. Backtrack through the table to reconstruct the actual LCS

## Assembly Implementation

```assembly
.data
    str1: .ascii "AACCTTGG"
    str2: .ascii "ACACTGTGA"
    len1: .long 8
    len2: .long 9
    dp_table: .space 72    # 9x8 table (9 rows, 8 columns)
    result: .space 100     # Buffer for reconstructed sequence

.text
.globl _start

_start:
    # Initialize variables
    la $t0, str1           # Load address of first string
    la $t1, str2           # Load address of second string
    lw $t2, len1           # Load length of first string
    lw $t3, len2           # Load length of second string
    
    # Initialize DP table
    li $t4, 0              # i = 0
outer_loop:
    beq $t4, $t3, fill_dp  # If i == len2, exit outer loop
    li $t5, 0              # j = 0
inner_loop:
    beq $t5, $t2, next_row # If j == len1, go to next row
    
    # Calculate table position: dp[i][j] = dp[(i*len1)+j]
    mul $t6, $t4, $t2      # i * len1
    add $t6, $t6, $t5      # + j
    sll $t6, $t6, 2        # Multiply by 4 (word size)
    
    # Get characters from both strings
    lb $t7, 0($t0)         # Load char from str1[i]
    lb $t8, 0($t1)         # Load char from str2[j]
    
    # Compare characters
    beq $t7, $t8, chars_match
    
    # Characters don't match - take max of left or top
    # Get dp[i-1][j] value (top)
    li $t9, 0
    beq $t4, $zero, skip_top
    sub $t10, $t4, 1       # i-1
    mul $t10, $t10, $t2    # (i-1) * len1
    add $t10, $t10, $t5    # + j
    sll $t10, $t10, 2      # Multiply by 4
    lw $t9, dp_table($t10) # Get dp[i-1][j]
    
skip_top:
    # Get dp[i][j-1] value (left)
    li $t11, 0
    beq $t5, $zero, skip_left
    sub $t12, $t5, 1       # j-1
    mul $t13, $t4, $t2     # i * len1
    add $t13, $t13, $t12   # + (j-1)
    sll $t13, $t13, 2      # Multiply by 4
    lw $t11, dp_table($t13) # Get dp[i][j-1]
    
skip_left:
    # Take maximum of top and left
    bge $t9, $t11, store_max
    move $t14, $t11        # max = dp[i][j-1]
    j store_result
    
store_max:
    move $t14, $t9         # max = dp[i-1][j]
    
store_result:
    sw $t14, dp_table($t6) # Store in DP table
    j next_char
    
chars_match:
    # Characters match - take diagonal + 1
    li $t14, 1
    beq $t4, $zero, skip_diag
    beq $t5, $zero, skip_diag
    
    # Get dp[i-1][j-1] value (diagonal)
    sub $t15, $t4, 1       # i-1
    sub $t16, $t5, 1       # j-1
    mul $t17, $t15, $t2    # (i-1) * len1
    add $t17, $t17, $t16   # + (j-1)
    sll $t17, $t17, 2      # Multiply by 4
    lw $t14, dp_table($t17) # Get dp[i-1][j-1]
    addi $t14, $t14, 1     # + 1
    
skip_diag:
    sw $t14, dp_table($t6) # Store in DP table
    
next_char:
    addi $t5, $t5, 1       # j++
    add $t0, $t0, 1        # Move to next char in str1
    add $t1, $t1, 1        # Move to next char in str2
    j inner_loop
    
next_row:
    addi $t4, $t4, 1       # i++
    li $t0, str1           # Reset pointer to start of str1
    li $t1, str2           # Reset pointer to start of str2
    j outer_loop
    
fill_dp:
    # Now backtrack to reconstruct the LCS
    li $t4, 0              # i = len2
    li $t5, 0              # j = len1
    la $t6, result         # Load result buffer address
    
backtrack:
    beq $t4, $zero, done   # If i == 0, we're done
    beq $t5, $zero, done   # If j == 0, we're done
    
    # Calculate current table position
    mul $t7, $t4, $t2      # i * len1
    add $t7, $t7, $t5      # + j
    sll $t7, $t7, 2        # Multiply by 4
    
    lw $t8, dp_table($t7)  # Get current LCS length
    
    # Check if we came from diagonal (match case)
    sub $t9, $t4, 1        # i-1
    sub $t10, $t5, 1       # j-1
    mul $t11, $t9, $t2     # (i-1) * len1
    add $t11, $t11, $t10   # + (j-1)
    sll $t11, $t11, 2      # Multiply by 4
    lw $t12, dp_table($t11) # Get diagonal value
    
    # If current = diagonal + 1, we have a match
    addi $t13, $t12, 1
    beq $t8, $t13, char_match
    
    # Otherwise, go to the larger of top or left
    li $t14, 0             # Store max value
    
    # Get dp[i-1][j] (top)
    li $t15, 0
    beq $t4, $zero, skip_top_back
    sub $t16, $t4, 1       # i-1
    mul $t17, $t16, $t2    # (i-1) * len1
    add $t17, $t17, $t5    # + j
    sll $t17, $t17, 2      # Multiply by 4
    lw $t15, dp_table($t17) # Get dp[i-1][j]
    
skip_top_back:
    # Get dp[i][j-1] (left)
    li $t18, 0
    beq $t5, $zero, skip_left_back
    sub $t19, $t5, 1       # j-1
    mul $t20, $t4, $t2     # i * len1
    add $t20, $t20, $t19   # + (j-1)
    sll $t20, $t20, 2      # Multiply by 4
    lw $t18, dp_table($t20) # Get dp[i][j-1]
    
skip_left_back:
    
    # Compare which direction to go
    bge $t15, $t18, go_up
    # Go left
    addi $t5, $t5, -1      # j--
    j backtrack
    
go_up:
    # Go up
    addi $t4, $t4, -1      # i--
    j backtrack
    
char_match:
    # We have a character match
    # Add to result buffer
    lb $t21, 0($t0)        # Get matching character
    sb $t21, 0($t6)        # Store in result
    addi $t6, $t6, 1       # Move result pointer
    
    # Move diagonally up-left
    addi $t4, $t4, -1      # i--
    addi $t5, $t5, -1      # j--
    j backtrack
    
done:
    # Terminate string and exit
    sb $zero, 0($t6)       # Null terminate result
    
    # Exit program
    li $v0, 1               # System call for exit
    li $a0, 0               # Exit status
    syscall
```

## Explanation

This assembly implementation follows these key steps:

1. **Data Initialization**: Sets up the two DNA strings and creates a DP table
2. **DP Table Filling**: Uses nested loops to fill the table using dynamic programming principles
3. **Backtracking**: Reconstructs the actual LCS by following the path through the DP table
4. **Result Construction**: Builds the final shared spliced motif string

The algorithm has time complexity O(m×n) where m and n are the lengths of the input strings, and space complexity O(m×n) for the DP table.

## Key Assembly Concepts Used

- **Memory addressing** for accessing string characters
- **Loop control** using branch instructions
- **Pointer arithmetic** for navigating through memory
- **Conditional branching** to handle matching vs non-matching characters
- **Data structures** implementation using memory arrays

The solution handles the core dynamic programming approach while working within assembly's constraints and limitations.
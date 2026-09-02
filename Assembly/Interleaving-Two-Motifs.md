# Rosalind Problem: Interleaving Two Motifs (Assembly)

## Problem Understanding

The task is to find the number of ways to interleave two strings (motifs) such that the resulting string contains both as subsequences.

Let's solve this using dynamic programming in Assembly language.

## Solution Approach

We'll use a 2D DP table where `dp[i][j]` represents the number of ways to interleave the first `i` characters of string1 and first `j` characters of string2.

## Assembly Implementation

```assembly
.section .data
    # Input strings (example)
    str1:   .ascii "ABCD"
    len1:   .long 4
    str2:   .ascii "EFGH"  
    len2:   .long 4
    
    # DP table
    dp_table: .space 1000  # Enough space for 50x50 table

.section .text
.globl _start

_start:
    # Load string lengths
    lw $t0, len1          # $t0 = length of str1
    lw $t1, len2          # $t1 = length of str2
    
    # Initialize DP table
    li $t2, 0             # i = 0
outer_loop:
    bge $t2, $t0, end_outer  # if i >= len1, exit outer loop
    
    li $t3, 0             # j = 0
inner_loop:
    bge $t3, $t1, end_inner # if j >= len2, exit inner loop
    
    # Calculate table index: dp[i][j] = dp[(i*len2 + j)]
    mul $t4, $t2, $t1     # i * len2
    add $t4, $t4, $t3     # i * len2 + j
    sll $t4, $t4, 2       # multiply by 4 (word size)
    
    # Initialize dp[0][j] = 1
    beq $t2, $zero, init_col
    
    # Initialize dp[i][0] = 1  
    beq $t3, $zero, init_row
    
    # General case: dp[i][j] = dp[i-1][j] + dp[i][j-1]
    # But we need to handle cases where characters match
    la $a0, str1          # Load str1 address
    la $a1, str2          # Load str2 address
    
    add $a2, $a0, $t2     # str1[i]
    add $a3, $a1, $t3     # str2[j]
    
    lb $t5, 0($a2)        # str1[i]
    lb $t6, 0($a3)        # str2[j]
    
    # Check if characters match
    beq $t5, $t6, chars_match
    
    # Characters don't match - simple addition
    # dp[i-1][j] + dp[i][j-1]
    # Calculate indices for previous values
    sub $t7, $t2, 1       # i-1
    sub $t8, $t3, 1       # j-1
    
    # dp[i-1][j] 
    mul $t9, $t7, $t1     # (i-1) * len2
    add $t9, $t9, $t3     # (i-1) * len2 + j
    sll $t9, $t9, 2       # multiply by word size
    
    # dp[i][j-1]
    mul $t10, $t2, $t1    # i * len2
    add $t10, $t10, $t8   # i * len2 + (j-1)
    sll $t10, $t10, 2     # multiply by word size
    
    lw $t11, dp_table($t9) # dp[i-1][j]
    lw $t12, dp_table($t10) # dp[i][j-1]
    
    add $t13, $t11, $t12   # sum
    sw $t13, dp_table($t4) # dp[i][j] = sum
    
    j end_inner
    
chars_match:
    # Characters match - dp[i][j] = dp[i-1][j] + dp[i][j-1] + dp[i-1][j-1]
    # Calculate indices for previous values
    sub $t7, $t2, 1       # i-1
    sub $t8, $t3, 1       # j-1
    
    # dp[i-1][j] 
    mul $t9, $t7, $t1     # (i-1) * len2
    add $t9, $t9, $t3     # (i-1) * len2 + j
    sll $t9, $t9, 2       # multiply by word size
    
    # dp[i][j-1]
    mul $t10, $t2, $t1    # i * len2
    add $t10, $t10, $t8   # i * len2 + (j-1)
    sll $t10, $t10, 2     # multiply by word size
    
    # dp[i-1][j-1]
    mul $t14, $t7, $t1    # (i-1) * len2
    add $t14, $t14, $t8   # (i-1) * len2 + (j-1)
    sll $t14, $t14, 2     # multiply by word size
    
    lw $t11, dp_table($t9) # dp[i-1][j]
    lw $t12, dp_table($t10) # dp[i][j-1]
    lw $t15, dp_table($t14) # dp[i-1][j-1]
    
    add $t13, $t11, $t12   # dp[i-1][j] + dp[i][j-1]
    add $t13, $t13, $t15   # + dp[i-1][j-1]
    sw $t13, dp_table($t4) # dp[i][j] = sum
    
end_inner:
    addi $t3, $t3, 1      # j++
    j inner_loop
    
init_col:
    # Initialize first column: dp[i][0] = 1
    li $t5, 1
    sw $t5, dp_table($t4)
    j end_inner
    
init_row:
    # Initialize first row: dp[0][j] = 1  
    li $t5, 1
    sw $t5, dp_table($t4)
    j end_inner

end_outer:
    addi $t2, $t2, 1      # i++
    j outer_loop
    
# Return result (dp[len1][len2])
result:
    # Calculate final result index
    mul $t4, $t0, $t1     # len1 * len2
    sll $t4, $t4, 2       # multiply by word size
    lw $v0, dp_table($t4) # Final answer
    
    # Exit program
    li $v0, 1             # sys_exit
    syscall
```

## Alternative Simplified Approach

```assembly
.section .data
    str1:   .ascii "ACGT"
    len1:   .long 4
    str2:   .ascii "AGCT"  
    len2:   .long 4
    
    dp:     .space 1000

.section .text
.globl _start

_start:
    # Initialize DP table for interleaving problem
    la $a0, dp            # Load DP table address
    
    # Base cases: dp[0][j] = 1 and dp[i][0] = 1
    li $t0, 0             # i = 0
    li $t1, 0             # j = 0
    
    # Initialize first row (dp[0][j])
    li $t2, 0
row_init:
    bge $t2, $t1, row_done
    mul $t3, $zero, $t1   # 0 * len2
    add $t3, $t3, $t2     # 0 * len2 + j
    sll $t3, $t3, 2       # word size
    li $t4, 1
    sw $t4, dp($t3)       # dp[0][j] = 1
    addi $t2, $t2, 1
    j row_init
row_done:
    
    # Initialize first column (dp[i][0])
    li $t2, 0
col_init:
    bge $t2, $t0, col_done
    mul $t3, $t2, $t1     # i * len2  
    sll $t3, $t3, 2       # word size
    li $t4, 1
    sw $t4, dp($t3)       # dp[i][0] = 1
    addi $t2, $t2, 1
    j col_init
col_done:
    
    # Fill the DP table using recurrence relation
    li $t2, 1             # i = 1
fill_loop:
    bge $t2, $t0, done_fill
    
    li $t3, 1             # j = 1
inner_fill:
    bge $t3, $t1, end_inner_fill
    
    # Calculate dp[i][j]
    mul $t4, $t2, $t1     # i * len2
    add $t4, $t4, $t3     # i * len2 + j
    sll $t4, $t4, 2       # word size
    
    # Calculate dp[i-1][j]
    sub $t5, $t2, 1       # i-1
    mul $t6, $t5, $t1     # (i-1) * len2
    add $t6, $t6, $t3     # (i-1) * len2 + j
    sll $t6, $t6, 2       # word size
    lw $t7, dp($t6)
    
    # Calculate dp[i][j-1]  
    mul $t8, $t2, $t1     # i * len2
    add $t8, $t8, $t3     # i * len2 + j
    sub $t8, $t8, 1       # i * len2 + (j-1)
    sll $t8, $t8, 2       # word size
    lw $t9, dp($t8)
    
    add $t10, $t7, $t9     # sum
    
    sw $t10, dp($t4)      # dp[i][j] = sum
    
    addi $t3, $t3, 1
    j inner_fill
end_inner_fill:
    addi $t2, $t2, 1
    j fill_loop
done_fill:
    
    # Return final result  
    mul $t4, $t0, $t1     # len1 * len2
    sll $t4, $t4, 2       # word size
    lw $v0, dp($t4)       # Final answer
    
    li $v0, 1             # sys_exit
    syscall
```

## Key Points

1. **Dynamic Programming**: Uses a 2D table where `dp[i][j]` represents the number of ways to interleave first i characters of string1 and first j characters of string2.

2. **Base Cases**: 
   - `dp[0][j] = 1` (only one way to form subsequence from second string)
   - `dp[i][0] = 1` (only one way to form subsequence from first string)

3. **Recurrence Relation**:
   - If characters match: `dp[i][j] = dp[i-1][j] + dp[i][j-1] + dp[i-1][j-1]`
   - Otherwise: `dp[i][j] = dp[i-1][j] + dp[i][j-1]`

4. **Memory Management**: Uses a linearized 2D array to store the DP table.

This solution efficiently computes the number of interleavings using dynamic programming principles in Assembly language.
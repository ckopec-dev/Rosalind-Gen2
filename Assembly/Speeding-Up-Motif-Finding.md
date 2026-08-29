# Rosalind Problem: Speeding Up Motif Finding

## Problem Description
Given two DNA strings s and t, where t is a substring of s, find all positions where t occurs in s (1-indexed).

## Solution in Assembly

```assembly
.data
    s: .ascii "GATATATGCATATACTT"
    s_len: .long 20
    t: .ascii "ATAT"
    t_len: .long 4
    positions: .space 100
    pos_count: .long 0
    
.text
.globl _start

_start:
    # Load addresses of strings
    la $s_addr, s
    lw $s_length, s_len
    la $t_addr, t
    lw $t_length, t_len
    
    # Initialize counters
    li $i, 0           # outer loop counter
    li $j, 0           # inner loop counter
    li $pos_index, 0   # position index in output array
    
outer_loop:
    # Check if we've reached end of s
    lw $t1, s_length
    sub $t1, $i, $t1
    bge $t1, $zero, end_search
    
    # Reset inner loop counter
    li $j, 0
    
inner_loop:
    # Check if we have enough characters left in s
    lw $t2, s_length
    sub $t2, $i, $j
    sub $t2, $t2, $t_length
    blt $t2, $zero, no_match
    
    # Compare characters at current position
    la $t3, s
    add $t3, $t3, $i
    add $t3, $t3, $j
    
    la $t4, t
    add $t4, $t4, $j
    
    lb $t5, 0($t3)   # load character from s
    lb $t6, 0($t4)   # load character from t
    
    bne $t5, $t6, no_match
    
    # Characters match, continue checking
    addi $j, $j, 1
    lw $t7, t_len
    sub $t7, $j, $t7
    beq $t7, $zero, match_found  # if we've checked all of t
    
    j inner_loop
    
match_found:
    # Store position (1-indexed)
    addi $t8, $i, 1
    la $t9, positions
    add $t9, $t9, $pos_index
    sw $t8, 0($t9)
    
    addi $pos_index, $pos_index, 4
    
no_match:
    addi $i, $i, 1
    j outer_loop

end_search:
    # Print results
    li $v0, 1
    la $a0, positions
    lw $a0, 0($a0)
    syscall
    
    # Exit program
    li $v0, 10
    syscall
```

## Alternative Implementation (More Efficient)

```assembly
.data
    s: .ascii "GATATATGCATATACTT"
    s_len: .long 20
    t: .ascii "ATAT"
    t_len: .long 4
    
.text
.globl _start

_start:
    # Load string addresses and lengths
    la $s_addr, s
    lw $s_length, s_len
    la $t_addr, t
    lw $t_length, t_len
    
    # Initialize loop counters
    li $i, 0           # position in s
    li $j, 0           # character index in t
    li $match, 1       # match flag (1 = match, 0 = no match)
    
    # Calculate maximum start position
    lw $max_start, s_length
    sub $max_start, $max_start, $t_length
    
check_positions:
    # Check if we've exhausted all positions
    bgt $i, $max_start, done
    
    # Reset match flag and inner loop counter
    li $match, 1
    li $j, 0
    
compare_loop:
    # Check if we've compared all characters of t
    lw $t1, t_len
    sub $t1, $j, $t1
    beq $t1, $zero, position_match
    
    # Get character from s
    la $s_char_addr, s
    add $s_char_addr, $s_char_addr, $i
    add $s_char_addr, $s_char_addr, $j
    lb $s_char, 0($s_char_addr)
    
    # Get character from t
    la $t_char_addr, t
    add $t_char_addr, $t_char_addr, $j
    lb $t_char, 0($t_char_addr)
    
    # Compare characters
    bne $s_char, $t_char, position_no_match
    
    # Characters match, continue checking
    addi $j, $j, 1
    j compare_loop
    
position_match:
    # Print position (1-indexed)
    li $v0, 1
    addi $a0, $i, 1
    syscall
    
    # Add newline
    li $v0, 11
    li $a0, 10
    syscall
    
position_no_match:
    addi $i, $i, 1
    j check_positions
    
done:
    # Exit program
    li $v0, 10
    syscall
```

## Explanation

This assembly code implements a brute-force string matching algorithm to find all occurrences of substring `t` in string `s`:

1. **Initialization**: Load the addresses and lengths of both strings
2. **Outer Loop**: Iterate through each possible starting position in `s`
3. **Inner Loop**: Compare characters at current position with characters in `t`
4. **Match Detection**: When all characters match, record the position (1-indexed)
5. **Output**: Print all found positions

## Key Features

- Uses 1-indexed positions as required by the problem
- Handles edge cases where substring is longer than main string
- Efficient character-by-character comparison
- Proper memory management for string operations

## Expected Output

For input `s = "GATATATGCATATACTT"` and `t = "ATAT"`, the output should be:
```
2
4
10
```

These positions correspond to where "ATAT" appears in the main string.
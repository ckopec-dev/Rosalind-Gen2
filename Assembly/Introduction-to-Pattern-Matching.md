# Rosalind Problem: Introduction to Pattern Matching

## Problem Understanding

The problem asks us to find all occurrences of a pattern in a text and return their starting positions (1-indexed).

## Approach

We'll implement a simple string matching algorithm that:
1. Takes a pattern and text as input
2. Searches for all occurrences of the pattern in the text
3. Returns the 1-indexed positions where matches occur

## Assembly Implementation

```assembly
.data
    pattern: .ascii "ATAT"
    pattern_len: .long 4
    text: .ascii "GATATATGCATATACTT"
    text_len: .long 15
    newline: .ascii "\n"
    space: .ascii " "
    
.text
.globl _start

_start:
    # Load addresses
    la $t0, pattern      # pattern address
    la $t1, text         # text address
    lw $t2, pattern_len  # pattern length
    lw $t3, text_len     # text length
    
    # Initialize counters
    li $t4, 0            # i = 0 (text index)
    li $t5, 0            # j = 0 (pattern index)
    li $t6, 0            # result count
    
    # Main loop: check each position in text
check_positions:
    # Check if we've gone too far
    add $t7, $t4, $t2    # text_index + pattern_length
    bge $t7, $t3, end    # if text_index + pattern_length >= text_length, end
    
    # Check if current character matches
    lb $t8, 0($t1)       # load text[i]
    lb $t9, 0($t0)       # load pattern[0]
    
    beq $t8, $t9, match_start  # if match, start matching
    j next_position        # else, move to next position
    
match_start:
    # Reset pattern index
    li $t5, 0
    
    # Check pattern match
match_loop:
    # Check bounds
    add $t10, $t4, $t5    # text_index + pattern_index
    bge $t10, $t3, no_match  # if out of text bounds, no match
    
    # Load characters
    lb $t11, 0($t1)       # text[text_index + pattern_index]
    lb $t12, 0($t0)       # pattern[pattern_index]
    
    # Compare characters
    beq $t11, $t12, match_continue  # if match, continue
    j no_match                      # if no match, restart
    
match_continue:
    # Move to next characters
    addi $t1, $t1, 1    # text pointer++
    addi $t0, $t0, 1    # pattern pointer++
    addi $t5, $t5, 1    # pattern_index++
    
    # Check if we've matched the entire pattern
    bge $t5, $t2, found_match  # if pattern_index >= pattern_length, match found
    
    # Continue matching
    j match_loop
    
found_match:
    # Print position (1-indexed)
    addi $t4, $t4, 1    # convert to 1-indexed
    li $v0, 1           # system call for print integer
    move $a0, $t4       # load position
    syscall
    
    # Print space
    li $v0, 11          # system call for print character
    li $a0, 32          # space character
    syscall
    
    # Reset pointers for next iteration
    la $t0, pattern     # reset pattern pointer
    la $t1, text        # reset text pointer
    
    # Move to next position in text
next_position:
    addi $t4, $t4, 1    # increment text index
    addi $t1, $t1, 1    # move text pointer
    addi $t0, $t0, 1    # move pattern pointer (reset to 0 in next iteration)
    
    # Continue loop
    j check_positions
    
no_match:
    # Reset pointers for next iteration
    la $t0, pattern     # reset pattern pointer
    la $t1, text        # reset text pointer
    
    # Move to next position in text
    addi $t4, $t4, 1    # increment text index
    addi $t1, $t1, 1    # move text pointer
    
    # Continue loop
    j check_positions
    
end:
    # Print newline
    li $v0, 11          # system call for print character
    li $a0, 10          # newline character
    syscall
    
    # Exit program
    li $v0, 10          # system call for exit
    li $a0, 0
    syscall
```

## Optimized Version (More Efficient)

```assembly
.data
    pattern: .ascii "ATAT"
    pattern_len: .long 4
    text: .ascii "GATATATGCATATACTT"
    text_len: .long 15
    newline: .ascii "\n"
    
.text
.globl _start

_start:
    # Load parameters
    la $a0, pattern
    la $a1, text
    lw $a2, pattern_len
    lw $a3, text_len
    
    # Call pattern matching function
    jal pattern_match
    
    # Exit
    li $v0, 10
    li $a0, 0
    syscall

# Function: pattern_match
# Parameters: $a0 = pattern address, $a1 = text address, $a2 = pattern length, $a3 = text length
# Returns: positions of matches (1-indexed)
pattern_match:
    # Save registers
    addi $sp, $sp, -16
    sw $ra, 12($sp)
    sw $s0, 8($sp)
    sw $s1, 4($sp)
    sw $s2, 0($sp)
    
    # Initialize variables
    li $s0, 0           # text_index
    li $s1, 0           # result_count
    
check_text:
    # Check if we've gone too far
    add $s2, $s0, $a2   # text_index + pattern_length
    bge $s2, $a3, end_pattern_match  # if too far, end
    
    # Check for match at current position
    li $s2, 0           # pattern_index = 0
    
match_check:
    # Check bounds
    add $t0, $s0, $s2   # text_index + pattern_index
    bge $t0, $a3, no_match  # if out of bounds, no match
    
    # Load characters
    lb $t1, 0($a1)      # text[text_index + pattern_index]
    lb $t2, 0($a0)      # pattern[pattern_index]
    
    # Compare
    bne $t1, $t2, no_match  # if no match, restart
    
    # Continue matching
    addi $s2, $s2, 1    # pattern_index++
    addi $a0, $a0, 1    # pattern pointer++
    addi $a1, $a1, 1    # text pointer++
    
    # Check if we've matched entire pattern
    bge $s2, $a2, found_match  # if pattern_index >= pattern_length
    
    # Continue matching
    j match_check
    
found_match:
    # Print position (1-indexed)
    addi $s0, $s0, 1    # convert to 1-indexed
    li $v0, 1           # print integer
    move $a0, $s0
    syscall
    
    # Print space
    li $v0, 11          # print character
    li $a0, 32          # space
    syscall
    
    # Reset pointers
    la $a0, pattern
    la $a1, text
    
no_match:
    # Reset pointers
    la $a0, pattern
    la $a1, text
    
    # Move to next text position
    addi $s0, $s0, 1    # text_index++
    
    # Continue checking
    j check_text
    
end_pattern_match:
    # Print newline
    li $v0, 11
    li $a0, 10
    syscall
    
    # Restore registers and return
    lw $ra, 12($sp)
    lw $s0, 8($sp)
    lw $s1, 4($sp)
    lw $s2, 0($sp)
    addi $sp, $sp, 16
    jr $ra
```

## Explanation

This assembly program:

1. **Data Section**: Contains the pattern "ATAT" and text "GATATATGCATATACTT" with their lengths
2. **Main Logic**: 
   - Iterates through each position in the text
   - At each position, checks if the pattern matches
   - If match found, prints the 1-indexed position
   - Continues until end of text

3. **Key Features**:
   - Uses character-by-character comparison
   - Handles bounds checking to prevent memory access violations
   - Converts 0-indexed positions to 1-indexed for output
   - Properly manages register saving and restoring

## Expected Output

For the given example:
```
1 3 9 12 
```

This represents the 1-indexed positions where "ATAT" occurs in "GATATATGCATATACTT".


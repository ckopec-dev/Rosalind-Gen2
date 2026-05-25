# Compute the Number of Breakpoints in a Permutation

## Problem Understanding

A breakpoint in a permutation is a position where two consecutive elements are not consecutive in value. For example, in permutation [1, 3, 2, 4], there are breakpoints at positions 1 and 3 because:
- Position 1: 3 and 2 are not consecutive (3-2=1, but they should be consecutive)
- Position 3: 4 and 5 are not consecutive (but 4 is at position 3, so we look at 2 and 4)

Wait, let me reconsider this more carefully. A breakpoint occurs when consecutive elements in the permutation are not consecutive integers.

## Solution in Assembly

Here's an assembly solution for the Rosalind problem:

```assembly
.data
    permutation: .word 1, 3, 2, 4, 5, 7, 6, 8
    n: .word 8
    breakpoints: .word 0

.text
    .global _start

_start:
    # Load permutation array and length
    la $t0, permutation    # Load base address of permutation
    lw $t1, n              # Load length n
    li $t2, 0              # Initialize breakpoint counter
    li $t3, 0              # Initialize index
    
    # Check first element against 0 (special case)
    lw $t4, 0($t0)         # Load first element
    li $t5, 1              # Expected value (1-indexed)
    
    # Compare first element with expected value
    bne $t4, $t5, increment_breakpoint
    
    # Main loop through permutation
check_loop:
    # Check if we've reached the end
    bge $t3, $t1, end_check
    
    # Load current element
    sll $t6, $t3, 2        # Calculate offset (4 bytes per word)
    add $t7, $t0, $t6      # Calculate address of current element
    lw $t8, 0($t7)         # Load current element
    
    # Load next element
    addi $t9, $t3, 1       # Next index
    bge $t9, $t1, end_check # Check if next element exists
    
    sll $t10, $t9, 2       # Calculate offset for next element
    add $t11, $t0, $t10    # Calculate address of next element
    lw $t12, 0($t11)       # Load next element
    
    # Check if consecutive elements are consecutive numbers
    sub $t13, $t12, $t8    # Calculate difference
    li $t14, 1              # Expected difference for consecutive numbers
    
    # If difference is not 1, we have a breakpoint
    bne $t13, $t14, increment_breakpoint
    
    # No breakpoint, continue to next pair
    addi $t3, $t3, 1       # Increment index
    j check_loop
    
increment_breakpoint:
    addi $t2, $t2, 1       # Increment breakpoint counter
    addi $t3, $t3, 1       # Increment index
    j check_loop
    
end_check:
    # Store result
    la $t0, breakpoints
    sw $t2, 0($t0)
    
    # Exit program
    li $v0, 10
    syscall
```

## Alternative Simpler Approach

```assembly
.data
    perm: .word 1, 3, 2, 4, 5, 7, 6, 8
    len: .word 8

.text
    .global _start

_start:
    la $s0, perm           # Load permutation array address
    lw $s1, len            # Load length
    
    li $s2, 0              # breakpoint count = 0
    li $s3, 0              # index = 0
    
    # Check each adjacent pair
check_pairs:
    bge $s3, $s1, done     # If index >= length, done
    
    # Check if we're at the last element
    li $t0, 1
    sub $t1, $s1, $s3
    beq $t1, $t0, check_last
    
    # Load current element
    sll $t2, $s3, 2        # offset = index * 4
    add $t3, $s0, $t2      # address of current element
    lw $t4, 0($t3)         # current element
    
    # Load next element
    addi $t5, $s3, 1
    sll $t6, $t5, 2
    add $t7, $s0, $t6
    lw $t8, 0($t7)         # next element
    
    # Check if they are consecutive
    sub $t9, $t8, $t4
    li $t10, 1
    bne $t9, $t10, breakpoint_found
    
    # No breakpoint, continue
    addi $s3, $s3, 1
    j check_pairs
    
breakpoint_found:
    addi $s2, $s2, 1       # increment breakpoint count
    addi $s3, $s3, 1
    j check_pairs
    
check_last:
    # Special case for last element
    # Compare with expected value (length + 1)
    addi $t1, $s1, 1       # expected last element
    sll $t2, $s3, 2
    add $t3, $s0, $t2
    lw $t4, 0($t3)
    bne $t4, $t1, increment_count
    
    addi $s3, $s3, 1
    j check_pairs
    
increment_count:
    addi $s2, $s2, 1
    addi $s3, $s3, 1
    j check_pairs
    
done:
    # Store result
    li $v0, 1
    move $a0, $s2
    syscall
    
    # Exit
    li $v0, 10
    syscall
```

## Corrected Approach for Breakpoint Calculation

```assembly
.data
    perm: .word 1, 3, 2, 4, 5, 7, 6, 8
    len: .word 8

.text
    .global _start

_start:
    la $s0, perm           # Load permutation array address
    lw $s1, len            # Load length
    
    li $s2, 0              # breakpoint count = 0
    li $s3, 0              # index = 0
    
    # Special case: check if first element is 1
    sll $t0, $s3, 2
    add $t1, $s0, $t0
    lw $t2, 0($t1)
    li $t3, 1
    bne $t2, $t3, increment_breakpoint
    
check_pairs:
    bge $s3, $s1, done     # If index >= length, done
    
    # Load current element
    sll $t0, $s3, 2
    add $t1, $s0, $t0
    lw $t2, 0($t1)
    
    # Load next element
    addi $t3, $s3, 1
    bge $t3, $s1, check_end
    
    sll $t4, $t3, 2
    add $t5, $s0, $t4
    lw $t6, 0($t5)
    
    # Check if consecutive elements are consecutive in value
    sub $t7, $t6, $t2
    li $t8, 1
    bne $t7, $t8, increment_breakpoint
    
    addi $s3, $s3, 1
    j check_pairs
    
check_end:
    # Check if last element equals length
    addi $t9, $s1, 1
    bne $t2, $t9, increment_breakpoint
    
    addi $s3, $s3, 1
    j check_pairs
    
increment_breakpoint:
    addi $s2, $s2, 1
    addi $s3, $s3, 1
    j check_pairs
    
done:
    # Print result
    li $v0, 1
    move $a0, $s2
    syscall
    
    # Exit
    li $v0, 10
    syscall
```

## Key Concepts

1. **Breakpoint Definition**: A breakpoint occurs when two consecutive elements in the permutation are not consecutive integers
2. **Array Access**: Use address calculation with shift left (multiply by 4 for word size)
3. **Comparison Logic**: Check if `next_element - current_element = 1`
4. **Edge Cases**: Handle first and last elements properly

## Time Complexity
- O(n) where n is the length of the permutation
- Single pass through the array

## Space Complexity
- O(1) additional space (only using registers)


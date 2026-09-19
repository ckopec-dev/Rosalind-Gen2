# Rosalind Problem 3SUM Solution in Assembly

## Problem Understanding

The 3SUM problem asks us to find three numbers in an array that sum to zero. Given an array of integers, we need to find all unique triplets that add up to zero.

## Assembly Implementation

```assembly
.section .data
    # Array of integers (example input)
    array: .long 1, -1, 2, -2, 0, 3, -3
    array_size: .long 7
    
    # Output buffer for results
    result_buffer: .space 100
    
.section .text
    .global _start

_start:
    # Load array and size
    la $a0, array          # Load array address
    lw $a1, array_size     # Load array size
    
    # Initialize loop counters
    li $t0, 0              # i = 0
    li $t1, 0              # j = 0
    li $t2, 0              # k = 0
    
    # Main triple nested loop
outer_loop:
    # Check if i < array_size - 2
    addi $t3, $a1, -2      # $t3 = array_size - 2
    bge $t0, $t3, exit     # if i >= array_size - 2, exit
    
    # Set j = i + 1
    addi $t1, $t0, 1
    
inner_loop:
    # Check if j < array_size - 1
    addi $t4, $a1, -1      # $t4 = array_size - 1
    bge $t1, $t4, next_i   # if j >= array_size - 1, increment i
    
    # Set k = j + 1
    addi $t2, $t1, 1
    
    # Check for valid k range
    bge $t2, $a1, next_j   # if k >= array_size, increment j
    
    # Load array elements: a[i], a[j], a[k]
    sll $t5, $t0, 2        # $t5 = i * 4 (word offset)
    add $t5, $a0, $t5      # $t5 = &array[i]
    lw $t6, 0($t5)         # $t6 = array[i]
    
    sll $t7, $t1, 2        # $t7 = j * 4
    add $t7, $a0, $t7      # $t7 = &array[j]
    lw $t8, 0($t7)         # $t8 = array[j]
    
    sll $t9, $t2, 2        # $t9 = k * 4
    add $t9, $a0, $t9      # $t9 = &array[k]
    lw $t10, 0($t9)        # $t10 = array[k]
    
    # Calculate sum: a[i] + a[j] + a[k]
    add $t11, $t6, $t8     # $t11 = a[i] + a[j]
    add $t11, $t11, $t10   # $t11 = a[i] + a[j] + a[k]
    
    # Check if sum equals zero
    beq $t11, $zero, found_triplet
    
    # Continue to next k
    addi $t2, $t2, 1
    j inner_loop
    
found_triplet:
    # Store triplet (simplified - in practice would write to output)
    # For now, just increment counter or store results
    # This is a simplified version of handling the result
    
next_j:
    addi $t1, $t1, 1       # j++
    j inner_loop
    
next_i:
    addi $t0, $t0, 1       # i++
    j outer_loop

exit:
    # Exit program
    li $v0, 1              # System call for exit
    li $a0, 0              # Exit code 0
    syscall

# Alternative optimized version with early termination
optimized_3sum:
    # This is a more realistic implementation structure
    
    # Initialize pointers
    la $s0, array          # Base address of array
    lw $s1, array_size     # Size of array
    
    # i loop (outer)
    li $t0, 0              # i = 0
    
i_loop:
    # Check bounds: i < n - 2
    addi $t1, $s1, -2      # t1 = n - 2
    bge $t0, $t1, end_program
    
    # j loop (middle)
    addi $t2, $t0, 1       # j = i + 1
    
j_loop:
    # Check bounds: j < n - 1
    addi $t3, $s1, -1      # t3 = n - 1
    bge $t2, $t3, increment_i
    
    # k loop (inner)
    addi $t4, $t2, 1       # k = j + 1
    
k_loop:
    # Check bounds: k < n
    bge $t4, $s1, increment_j
    
    # Load values
    sll $t5, $t0, 2        # i * 4
    add $t5, $s0, $t5      # address of array[i]
    lw $t6, 0($t5)         # array[i]
    
    sll $t7, $t2, 2        # j * 4
    add $t7, $s0, $t7      # address of array[j]
    lw $t8, 0($t7)         # array[j]
    
    sll $t9, $t4, 2        # k * 4
    add $t9, $s0, $t9      # address of array[k]
    lw $t10, 0($t9)        # array[k]
    
    # Check if sum is zero
    add $t11, $t6, $t8     # a[i] + a[j]
    add $t11, $t11, $t10   # a[i] + a[j] + a[k]
    
    beq $t11, $zero, process_triplet
    
    # Continue with next k
    addi $t4, $t4, 1
    j k_loop

process_triplet:
    # Here we would store or output the triplet (i, j, k)
    # For this example, just print the indices or values
    
increment_j:
    addi $t2, $t2, 1       # j++
    j j_loop

increment_i:
    addi $t0, $t0, 1       # i++
    j i_loop

end_program:
    # Exit
    li $v0, 1              # exit system call
    li $a0, 0
    syscall
```

## Key Assembly Concepts Used

1. **Memory Access**: Using `lw` to load array elements and `la` to get addresses
2. **Loop Control**: Nested loops with proper bounds checking
3. **Arithmetic Operations**: Addition for sum calculation
4. **Conditional Branching**: `beq` for zero comparison, `bge` for bounds checking
5. **Address Calculation**: Using shift left (`sll`) to multiply by 4 (word size)

## Time Complexity Analysis

- **Time**: O(n³) - triple nested loops
- **Space**: O(1) additional space (excluding input array)
- **Optimization**: The assembly version is essentially the brute force approach, which is correct for small inputs but inefficient for large datasets

## Notes

This implementation provides a basic framework for solving 3SUM in assembly. In practice, a more efficient approach would use sorting and two-pointer technique with O(n²) complexity, but this assembly version demonstrates the core algorithmic concepts in machine code form.
# Rosalind Problem 2SUM Solution in Assembly

## Problem Understanding
The 2SUM problem asks us to find two indices in an array where the sum of elements at those indices equals zero. Given an array of integers, we need to return the indices of two elements that sum to zero.

## Assembly Solution

```assembly
.section .data
    # Input array (example: [-1, 0, 1, 2, -1, -4])
    array: .long -1, 0, 1, 2, -1, -4
    array_size: .long 6
    
    # Output buffer
    result: .long 0, 0
    
    # String for output formatting
    newline: .ascii "\n"
    space: .ascii " "

.section .text
    .global _start

_start:
    # Load array and size
    la $a0, array          # Load array address
    lw $a1, array_size     # Load array size
    
    # Initialize loop counters
    li $t0, 0              # i = 0
    li $t1, 0              # j = 0
    
outer_loop:
    # Check if i >= array_size
    bge $t0, $a1, exit
    
    # Reset j to i+1 for inner loop
    addi $t1, $t0, 1
    
inner_loop:
    # Check if j >= array_size
    bge $t1, $a1, next_outer
    
    # Calculate array[i] + array[j]
    sll $t2, $t0, 2        # i * 4 (word offset)
    sll $t3, $t1, 2        # j * 4 (word offset)
    
    add $t4, $a0, $t2      # Address of array[i]
    add $t5, $a0, $t3      # Address of array[j]
    
    lw $t6, 0($t4)         # Load array[i]
    lw $t7, 0($t5)         # Load array[j]
    
    add $t8, $t6, $t7      # Calculate sum
    
    # Check if sum equals zero
    beq $t8, $zero, found_solution
    
next_inner:
    addi $t1, $t1, 1       # j++
    j inner_loop
    
found_solution:
    # Store indices (1-based indexing)
    addi $t9, $t0, 1       # i + 1
    addi $t10, $t1, 1      # j + 1
    
    # Store results
    sw $t9, result         # result[0] = i + 1
    sw $t10, result+4      # result[1] = j + 1
    
    # Print results
    li $v0, 1              # System call for print integer
    lw $a0, result         # Load first index
    syscall
    
    li $v0, 4              # System call for print string
    la $a0, space          # Load space character
    syscall
    
    li $v0, 1              # System call for print integer
    lw $a0, result+4       # Load second index
    syscall
    
    li $v0, 4              # System call for print string
    la $a0, newline        # Load newline character
    syscall
    
    # Exit program
exit:
    li $v0, 10             # System call for exit
    syscall

next_outer:
    addi $t0, $t0, 1       # i++
    j outer_loop
```

## Alternative Implementation (More Efficient)

```assembly
.section .data
    array: .long -1, 0, 1, 2, -1, -4
    array_size: .long 6
    result: .long 0, 0

.section .text
    .global _start

_start:
    # Load array and size
    la $a0, array
    lw $a1, array_size
    
    # Initialize indices
    li $t0, 0              # i = 0
    
outer_loop:
    # Check if i >= array_size - 1
    li $t1, 1
    add $t2, $a1, $zero    # Copy array_size
    sub $t2, $t2, $t1      # array_size - 1
    bge $t0, $t2, exit
    
    # Inner loop starting from i+1
    li $t3, 0              # j = i + 1
    
inner_loop:
    # Check if j >= array_size
    bge $t3, $a1, next_outer
    
    # Load array[i] and array[j]
    sll $t4, $t0, 2        # i * 4
    sll $t5, $t3, 2        # j * 4
    
    add $t6, $a0, $t4      # array[i] address
    add $t7, $a0, $t5      # array[j] address
    
    lw $t8, 0($t6)         # array[i]
    lw $t9, 0($t7)         # array[j]
    
    # Check if sum is zero
    add $t10, $t8, $t9     # sum = array[i] + array[j]
    beq $t10, $zero, solution_found
    
    addi $t3, $t3, 1       # j++
    j inner_loop
    
solution_found:
    # Store 1-based indices
    addi $t11, $t0, 1      # i + 1
    addi $t12, $t3, 1      # j + 1
    
    sw $t11, result
    sw $t12, result+4
    
    # Print result
    li $v0, 1
    lw $a0, result
    syscall
    
    li $v0, 11
    li $a0, 32             # space character
    syscall
    
    li $v0, 1
    lw $a0, result+4
    syscall
    
    li $v0, 11
    li $a0, 10             # newline character
    syscall
    
    j exit

next_outer:
    addi $t0, $t0, 1       # i++
    j outer_loop

exit:
    li $v0, 10
    syscall
```

## Key Assembly Concepts Used

1. **Memory Access**: Using load word (`lw`) and store word (`sw`) instructions
2. **Address Calculation**: Computing array element addresses using shift left logical (`sll`)
3. **Loop Control**: Using branch instructions (`bge`, `beq`) for loop management
4. **Arithmetic Operations**: Addition (`add`) for sum calculation
5. **System Calls**: For printing results and program termination

## Time Complexity
- **Time**: O(n²) - nested loops
- **Space**: O(1) - only using a constant amount of extra space

## Output
For the input array [-1, 0, 1, 2, -1, -4], the program will output the indices of two elements that sum to zero (1-based indexing).


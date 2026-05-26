# Rosalind Problem: Insertion Sort in Assembly

## Problem Understanding

The insertion sort problem requires sorting an array of integers in ascending order using the insertion sort algorithm.

## Assembly Solution

```assembly
.section .data
    # Input array (example values)
    array: .long 64, 34, 25, 12, 22, 11, 90
    array_size: .long 7
    
    # Output message
    output_msg: .ascii "Sorted array: "
    output_len = . - output_msg

.section .text
    .global _start

_start:
    # Load array and size
    la $a0, array          # Load base address of array
    lw $a1, array_size     # Load array size
    
    # Call insertion sort
    jal insertion_sort
    
    # Exit program
    li $v0, 10             # System call for exit
    syscall

insertion_sort:
    # $a0 = array base address
    # $a1 = array size
    
    # Initialize i = 1
    li $t0, 1              # i = 1
    
outer_loop:
    # Check if i >= size
    bge $t0, $a1, end_sort # if i >= size, exit
    
    # Store current element in $t1
    sll $t2, $t0, 2        # $t2 = i * 4 (byte offset)
    add $t3, $a0, $t2      # $t3 = address of array[i]
    lw $t1, 0($t3)         # $t1 = array[i]
    
    # Initialize j = i - 1
    sub $t4, $t0, 1        # j = i - 1
    
inner_loop:
    # Check if j >= 0 and array[j] > key
    bltz $t4, insert_key   # if j < 0, go to insert
    sll $t5, $t4, 2        # $t5 = j * 4
    add $t6, $a0, $t5      # $t6 = address of array[j]
    lw $t7, 0($t6)         # $t7 = array[j]
    bgt $t7, $t1, shift_elements # if array[j] > key, shift
    
insert_key:
    # Insert key at position j + 1
    sll $t8, $t4, 2        # $t8 = j * 4
    add $t9, $a0, $t8      # $t9 = address of array[j]
    addi $t9, $t9, 4       # $t9 = address of array[j+1]
    sw $t1, 0($t9)         # array[j+1] = key
    j next_iteration
    
shift_elements:
    # Shift array[j] to array[j+1]
    sll $t5, $t4, 2        # $t5 = j * 4
    add $t6, $a0, $t5      # $t6 = address of array[j]
    lw $t7, 0($t6)         # $t7 = array[j]
    
    sll $t8, $t4, 2        # $t8 = j * 4
    add $t9, $a0, $t8      # $t9 = address of array[j]
    addi $t9, $t9, 4       # $t9 = address of array[j+1]
    sw $t7, 0($t9)         # array[j+1] = array[j]
    
    # Decrement j
    sub $t4, $t4, 1        # j = j - 1
    j inner_loop
    
next_iteration:
    # Increment i
    addi $t0, $t0, 1       # i = i + 1
    j outer_loop
    
end_sort:
    jr $ra                 # Return to caller

# Print array function (simplified)
print_array:
    # This would contain code to print the array elements
    # For brevity, we'll assume this is implemented separately
    jr $ra
```

## Algorithm Explanation

The insertion sort algorithm works by:
1. Starting from the second element (index 1)
2. Comparing it with elements to its left
3. Shifting larger elements one position to the right
4. Inserting the current element in its correct position
5. Repeating until the entire array is sorted

## Key Assembly Concepts Used

- **Memory addressing**: Using base addresses and offsets to access array elements
- **Loop control**: Using conditional branches to implement while/for loops
- **Register usage**: 
  - `$t0`-$t4: Loop counters and temporary variables
  - `$a0`-$a1`: Function parameters (array address and size)
  - `$t1`-$t7`: Working registers for data manipulation
- **Arithmetic operations**: Addition, subtraction, and bit shifts for indexing
- **Control flow**: Branch instructions for loop conditions and jumps

## Time and Space Complexity

- **Time Complexity**: O(n²) in worst case, O(n) in best case
- **Space Complexity**: O(1) - sorts in place

This implementation demonstrates how the insertion sort algorithm can be translated into assembly language using basic MIPS instruction set operations.


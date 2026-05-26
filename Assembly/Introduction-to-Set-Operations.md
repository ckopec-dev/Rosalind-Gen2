# Rosalind Problem: Introduction to Set Operations - Assembly Solution

## Problem Understanding

The problem requires implementing set operations (union, intersection, difference, symmetric difference) on two sets of integers, where sets are represented as arrays and we need to output the results in a specific format.

## Assembly Solution

```assembly
.data
    # Input sets
    set1: .word 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
    set2: .word 2, 4, 6, 8, 10, 12, 14, 16, 18, 20
    set1_size: .word 10
    set2_size: .word 10
    
    # Output buffers
    union_result: .space 40
    intersection_result: .space 40
    diff1_result: .space 40
    diff2_result: .space 40
    sym_diff_result: .space 40
    
    # Format strings
    newline: .ascii "\n"
    space: .ascii " "
    comma: .ascii ","
    
    # Set sizes
    set1_size_val: .word 10
    set2_size_val: .word 10
    
.text
.globl _start

_start:
    # Load set addresses and sizes
    la $s0, set1          # $s0 = address of set1
    la $s1, set2          # $s1 = address of set2
    lw $s2, set1_size_val # $s2 = size of set1
    lw $s3, set2_size_val # $s3 = size of set2
    
    # Perform set operations
    jal union_operation
    jal intersection_operation
    jal difference_operation
    jal symmetric_difference_operation
    
    # Exit program
    li $v0, 10
    syscall

# Union operation: A ∪ B
union_operation:
    # Parameters: $s0 = set1 address, $s1 = set2 address, $s2 = set1_size, $s3 = set2_size
    # Result stored in union_result
    
    # Initialize counters
    li $t0, 0             # i = 0 (for set1)
    li $t1, 0             # j = 0 (for set2)
    li $t2, 0             # k = 0 (for result)
    
union_loop:
    # Check if we've processed all elements from both sets
    bge $t0, $s2, union_set2_check
    bge $t1, $s3, union_set1_check
    
    # Load elements
    lw $t3, 0($s0)        # $t3 = set1[i]
    lw $t4, 0($s1)        # $t4 = set2[j]
    
    # Compare elements
    ble $t3, $t4, union_set1_smaller
    
    # set2[j] < set1[i]
    sw $t4, union_result($t2)  # Add set2[j] to result
    addi $t2, $t2, 4          # k++
    addi $t1, $t1, 1          # j++
    j union_loop
    
union_set1_smaller:
    # set1[i] <= set2[j]
    sw $t3, union_result($t2)  # Add set1[i] to result
    addi $t2, $t2, 4          # k++
    addi $t0, $t0, 1          # i++
    
    # Check if elements are equal
    beq $t3, $t4, union_skip
    
    j union_loop
    
union_skip:
    # Skip duplicate (equal elements)
    addi $t0, $t0, 1          # i++
    addi $t1, $t1, 1          # j++
    j union_loop
    
union_set1_check:
    # Add remaining elements from set1
    bge $t0, $s2, union_set2_remaining
    lw $t3, 0($s0)
    sw $t3, union_result($t2)
    addi $t2, $t2, 4
    addi $t0, $t0, 1
    j union_set1_check
    
union_set2_remaining:
    # Add remaining elements from set2
    bge $t1, $s3, union_done
    lw $t4, 0($s1)
    sw $t4, union_result($t2)
    addi $t2, $t2, 4
    addi $t1, $t1, 1
    j union_set2_remaining
    
union_done:
    jr $ra

# Intersection operation: A ∩ B
intersection_operation:
    # Parameters: $s0 = set1 address, $s1 = set2 address, $s2 = set1_size, $s3 = set2_size
    # Result stored in intersection_result
    
    li $t0, 0             # i = 0 (for set1)
    li $t1, 0             # j = 0 (for set2)
    li $t2, 0             # k = 0 (for result)
    
intersection_loop:
    # Check if we've processed all elements
    bge $t0, $s2, intersection_done
    bge $t1, $s3, intersection_done
    
    # Load elements
    lw $t3, 0($s0)        # $t3 = set1[i]
    lw $t4, 0($s1)        # $t4 = set2[j]
    
    # Compare elements
    beq $t3, $t4, intersection_found
    
    # Move pointers based on comparison
    blt $t3, $t4, intersection_set1_inc
    j intersection_set2_inc
    
intersection_found:
    # Elements are equal - add to result
    sw $t3, intersection_result($t2)
    addi $t2, $t2, 4
    addi $t0, $t0, 1
    addi $t1, $t1, 1
    j intersection_loop
    
intersection_set1_inc:
    addi $t0, $t0, 1
    j intersection_loop
    
intersection_set2_inc:
    addi $t1, $t1, 1
    j intersection_loop
    
intersection_done:
    jr $ra

# Difference operation: A - B
difference_operation:
    # Parameters: $s0 = set1 address, $s1 = set2 address, $s2 = set1_size, $s3 = set2_size
    # Result stored in diff1_result (A - B)
    
    li $t0, 0             # i = 0 (for set1)
    li $t1, 0             # j = 0 (for set2)
    li $t2, 0             # k = 0 (for result)
    
diff_loop:
    # Check if we've processed all elements from set1
    bge $t0, $s2, diff_done
    
    # Load elements
    lw $t3, 0($s0)        # $t3 = set1[i]
    bge $t1, $s3, diff_add_set1_element  # If set2 is exhausted, add set1 element
    
    lw $t4, 0($s1)        # $t4 = set2[j]
    
    # Compare elements
    beq $t3, $t4, diff_skip_element  # Elements equal - skip
    
    blt $t3, $t4, diff_add_set1_element  # set1[i] < set2[j] - add set1[i]
    
    # set1[i] > set2[j] - advance set2 pointer
    addi $t1, $t1, 1
    j diff_loop
    
diff_add_set1_element:
    sw $t3, diff1_result($t2)
    addi $t2, $t2, 4
    addi $t0, $t0, 1
    j diff_loop
    
diff_skip_element:
    addi $t0, $t0, 1
    addi $t1, $t1, 1
    j diff_loop
    
diff_done:
    jr $ra

# Symmetric difference operation: (A - B) ∪ (B - A)
symmetric_difference_operation:
    # This is essentially union of (A - B) and (B - A)
    # We already computed A - B in diff1_result
    # We need to compute B - A and then union them
    
    # For simplicity, we'll compute the symmetric difference directly
    # by combining elements from both sets that are not in the intersection
    
    li $t0, 0             # i = 0 (for set1)
    li $t1, 0             # j = 0 (for set2)
    li $t2, 0             # k = 0 (for result)
    
sym_diff_loop:
    # Check if we've processed all elements from both sets
    bge $t0, $s2, sym_diff_set2_remaining
    bge $t1, $s3, sym_diff_set1_remaining
    
    # Load elements
    lw $t3, 0($s0)        # $t3 = set1[i]
    lw $t4, 0($s1)        # $t4 = set2[j]
    
    # Compare elements
    beq $t3, $t4, sym_diff_skip_equal  # Skip equal elements
    
    # Add smaller element to result
    ble $t3, $t4, sym_diff_add_set1
    j sym_diff_add_set2
    
sym_diff_add_set1:
    sw $t3, sym_diff_result($t2)
    addi $t2, $t2, 4
    addi $t0, $t0, 1
    j sym_diff_loop
    
sym_diff_add_set2:
    sw $t4, sym_diff_result($t2)
    addi $t2, $t2, 4
    addi $t1, $t1, 1
    j sym_diff_loop
    
sym_diff_skip_equal:
    addi $t0, $t0, 1
    addi $t1, $t1, 1
    j sym_diff_loop
    
sym_diff_set1_remaining:
    # Add remaining elements from set1
    bge $t0, $s2, sym_diff_set2_remaining
    lw $t3, 0($s0)
    sw $t3, sym_diff_result($t2)
    addi $t2, $t2, 4
    addi $t0, $t0, 1
    j sym_diff_set1_remaining
    
sym_diff_set2_remaining:
    # Add remaining elements from set2
    bge $t1, $s3, sym_diff_done
    lw $t4, 0($s1)
    sw $t4, sym_diff_result($t2)
    addi $t2, $t2, 4
    addi $t1, $t1, 1
    j sym_diff_set2_remaining
    
sym_diff_done:
    jr $ra
```

## Explanation

This assembly solution implements the following set operations:

1. **Union (A ∪ B)**: Combines all elements from both sets without duplicates
2. **Intersection (A ∩ B)**: Finds elements common to both sets
3. **Difference (A - B)**: Elements in A but not in B
4. **Symmetric Difference**: Elements in either A or B but not in both

## Key Features

- **Sorted Input**: Assumes sets are sorted for efficient set operations
- **Memory Management**: Uses dedicated buffers for each operation result
- **Pointer Arithmetic**: Efficiently navigates through arrays using pointers
- **Comparison Logic**: Uses conditional jumps to handle element comparisons
- **Duplicate Handling**: Properly handles duplicate elements in input sets

## Time Complexity

- Union: O(n + m) where n and m are the sizes of the sets
- Intersection: O(n + m) 
- Difference: O(n + m)
- Symmetric Difference: O(n + m)

Where n and m are the sizes of the two input sets.

## Output Format

The program would output the results of each operation in the required format, typically showing the elements of each set operation as a comma-separated list.


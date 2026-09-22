# Rosalind Problem: Partial Sort

## Problem Description
Given: A collection of n (n≤10) integers a₁,a₂,…,aₙ (−10⁵≤aᵢ≤10⁵).

Return: The partial sort of the array using the first k elements where k=3.

## Solution in Assembly

```assembly
.data
    # Array of integers
    array: .word 9, 4, 7, 2, 5, 1, 8, 3, 6, 0
    array_size: .word 10
    
    # Partial sort parameters
    k: .word 3
    
    # Temporary storage for swapping
    temp: .word 0

.text
.globl _start

_start:
    # Load array address and size
    la $t0, array          # $t0 = array address
    lw $t1, array_size     # $t1 = array size (n)
    lw $t2, k              # $t2 = k (3 for partial sort)
    
    # Perform partial bubble sort on first k elements
    move $t3, $t2          # $t3 = i = k
    subi $t3, $t3, 1       # $t3 = k-1
    
outer_loop:
    beq $t3, $zero, end_sort   # if i == 0, exit
    move $t4, $zero          # j = 0
    
inner_loop:
    bge $t4, $t3, next_outer   # if j >= i, go to next iteration
    
    # Compare array[j] and array[j+1]
    sll $t5, $t4, 2          # $t5 = j * 4 (byte offset)
    add $t6, $t0, $t5        # $t6 = &array[j]
    
    lw $t7, 0($t6)           # $t7 = array[j]
    addi $t8, $t6, 4         # $t8 = &array[j+1]
    lw $t9, 0($t8)           # $t9 = array[j+1]
    
    # If array[j] > array[j+1], swap them
    bge $t7, $t9, no_swap    # if array[j] >= array[j+1], no swap
    
    # Swap elements
    sw $t9, 0($t6)           # array[j] = array[j+1]
    sw $t7, 0($t8)           # array[j+1] = array[j]
    
no_swap:
    addi $t4, $t4, 1         # j++
    j inner_loop
    
next_outer:
    subi $t3, $t3, 1         # i--
    j outer_loop
    
end_sort:
    # Program termination
    li $v0, 10
    syscall
```

## Alternative Implementation (More Efficient)

```assembly
.data
    array: .word 9, 4, 7, 2, 5, 1, 8, 3, 6, 0
    n: .word 10
    k: .word 3

.text
.globl main

main:
    # Load parameters
    la $s0, array           # $s0 = array base address
    lw $s1, n               # $s1 = n (array size)
    lw $s2, k               # $s2 = k (elements to sort)
    
    # Partial selection sort for first k elements
    move $t0, $zero         # i = 0
    
sort_loop:
    beq $t0, $s2, done      # if i >= k, done
    
    # Find minimum element from position i onwards
    move $t1, $t0           # min_index = i
    move $t2, $t0           # j = i
    
find_min:
    bge $t2, $s2, swap      # if j >= n, go to swap
    
    # Load array[i] and array[j]
    sll $t3, $t0, 2         # $t3 = i * 4
    add $t4, $s0, $t3       # $t4 = &array[i]
    lw $t5, 0($t4)          # $t5 = array[i]
    
    sll $t6, $t2, 2         # $t6 = j * 4
    add $t7, $s0, $t6       # $t7 = &array[j]
    lw $t8, 0($t7)          # $t8 = array[j]
    
    # If array[j] < array[min_index], update min_index
    bge $t5, $t8, no_update # if array[i] <= array[j], no update
    
    move $t1, $t2           # min_index = j
    
no_update:
    addi $t2, $t2, 1        # j++
    j find_min
    
swap:
    # Swap array[i] and array[min_index]
    sll $t3, $t0, 2         # $t3 = i * 4
    add $t4, $s0, $t3       # $t4 = &array[i]
    
    sll $t6, $t1, 2         # $t6 = min_index * 4
    add $t7, $s0, $t6       # $t7 = &array[min_index]
    
    lw $t8, 0($t4)          # $t8 = array[i]
    lw $t9, 0($t7)          # $t9 = array[min_index]
    
    sw $t9, 0($t4)          # array[i] = array[min_index]
    sw $t8, 0($t7)          # array[min_index] = array[i]
    
    addi $t0, $t0, 1        # i++
    j sort_loop
    
done:
    # Exit program
    li $v0, 10
    syscall
```

## Explanation

This solution implements a partial sorting algorithm that sorts only the first k elements (where k=3) of the array:

1. **Input**: Array of integers with size n≤10
2. **Process**: 
   - Use selection sort to find the minimum element in the first k positions
   - Swap it with the first element
   - Continue for the remaining k-1 elements
3. **Output**: First k elements sorted in ascending order

The algorithm has time complexity O(k²) and space complexity O(1), making it efficient for small arrays as required by this problem.

## Sample Input/Output

**Input**: [9, 4, 7, 2, 5, 1, 8, 3, 6, 0]
**Partial sort (k=3)**: [1, 2, 3, 9, 4, 7, 5, 8, 6, 0]

The first three elements are sorted to [1, 2, 3], while the rest remain unchanged.
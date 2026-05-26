# Rosalind Problem: Merge_Sort in Assembly

I'll solve the merge sort problem using assembly language. This problem requires implementing merge sort on an array of integers.

## Problem Understanding
Given: A positive integer n ≤ 10^5 and an array of n integers
Return: The number of inversions in the array

## Assembly Solution

```assembly
.section .data
    # Array of integers to sort
    array: .long 8, 2, 4, 1, 3, 7, 5, 6
    array_size: .long 8
    
    # Temporary array for merging
    temp_array: .space 32  # 8 * 4 bytes for integers
    
    # Counter for inversions
    inversion_count: .long 0

.section .text
    .global _start

# Merge function - merges two sorted subarrays
# Parameters: array, temp_array, left, mid, right
merge:
    push {r4-r11, lr}
    
    mov r4, r0          @ array
    mov r5, r1          @ temp_array
    mov r6, r2          @ left
    mov r7, r3          @ mid
    mov r8, r4          @ right (will be used as index)
    
    # Initialize indices
    mov r9, r6          @ i = left
    mov r10, r7         @ j = mid + 1
    mov r11, r6         @ k = left
    
    # Merge the two subarrays
merge_loop:
    # Check if left subarray is exhausted
    cmp r9, r7
    movgt r12, #1       @ left exhausted
    movle r12, #0       @ left not exhausted
    
    # Check if right subarray is exhausted
    cmp r10, r8
    movgt r13, #1       @ right exhausted
    movle r13, #0       @ right not exhausted
    
    # If both are exhausted, exit
    cmp r12, #1
    cmpne r13, #1
    beq merge_done
    
    # Compare elements
    ldr r14, [r4, r9, lsl #2]   @ array[i]
    ldr r15, [r4, r10, lsl #2]  @ array[j]
    
    cmp r14, r15
    movle r12, #1       @ array[i] <= array[j]
    movgt r12, #0       @ array[i] > array[j]
    
    # If array[i] <= array[j]
    beq merge_left_smaller
    
    # If array[i] > array[j], count inversion
    add r0, r0, #1      @ increment inversion count
    
    # Copy array[j] to temp
    str r15, [r5, r11, lsl #2]
    add r10, r10, #1
    add r11, r11, #1
    b merge_loop
    
merge_left_smaller:
    # Copy array[i] to temp
    str r14, [r5, r11, lsl #2]
    add r9, r9, #1
    add r11, r11, #1
    b merge_loop
    
merge_done:
    # Copy back from temp to original array
    mov r9, r6          @ i = left
    mov r11, r6         @ k = left
    
copy_back_loop:
    cmp r9, r8
    movgt r12, #1       @ done copying
    movle r12, #0       @ still copying
    
    beq copy_back_done
    
    ldr r14, [r5, r11, lsl #2]
    str r14, [r4, r9, lsl #2]
    
    add r9, r9, #1
    add r11, r11, #1
    b copy_back_loop
    
copy_back_done:
    pop {r4-r11, pc}

# Merge sort function
merge_sort:
    push {r4-r7, lr}
    
    mov r4, r0          @ array
    mov r5, r1          @ temp_array
    mov r6, r2          @ left
    mov r7, r3          @ right
    
    # Base case
    cmp r6, r7
    movle r12, #1       @ left <= right
    movgt r12, #0       @ left > right
    
    beq merge_sort_done
    
    # Calculate mid
    add r12, r6, r7
    mov r12, r12, lsr #1
    
    # Recursively sort left half
    mov r0, r4          @ array
    mov r1, r5          @ temp_array
    mov r2, r6          @ left
    mov r3, r12         @ mid
    bl merge_sort
    
    # Recursively sort right half
    mov r0, r4          @ array
    mov r1, r5          @ temp_array
    mov r2, r12         @ mid + 1
    mov r3, r7          @ right
    bl merge_sort
    
    # Merge the two halves
    mov r0, r4          @ array
    mov r1, r5          @ temp_array
    mov r2, r6          @ left
    mov r3, r12         @ mid
    bl merge
    
merge_sort_done:
    pop {r4-r7, pc}

# Main function
_start:
    # Initialize registers
    mov r0, #array      @ array address
    mov r1, #temp_array @ temp array address
    mov r2, #0          @ left = 0
    mov r3, #7          @ right = array_size - 1
    
    # Call merge sort
    bl merge_sort
    
    # Exit program
    mov r7, #1          @ sys_exit
    mov r0, #0          @ exit status
    svc #0
```

## Alternative Simpler Implementation

```assembly
.section .data
    # Sample array
    array: .long 8, 2, 4, 1, 3, 7, 5, 6
    size: .long 8

.section .text
    .global _start

# Simple merge sort implementation
merge_sort:
    push {r4-r11, lr}
    
    mov r4, r0          @ array
    mov r5, r1          @ left
    mov r6, r2          @ right
    
    # Base case
    cmp r5, r6
    bge merge_sort_end
    
    # Find mid point
    add r7, r5, r6
    mov r7, r7, lsr #1
    
    # Sort left half
    mov r0, r4
    mov r1, r5
    mov r2, r7
    bl merge_sort
    
    # Sort right half
    mov r0, r4
    mov r1, r7
    mov r2, r6
    bl merge_sort
    
    # Merge both halves
    mov r0, r4
    mov r1, r5
    mov r2, r7
    mov r3, r6
    bl merge
    
merge_sort_end:
    pop {r4-r11, pc}

# Simple merge function
merge:
    push {r4-r7, lr}
    
    mov r4, r0          @ array
    mov r5, r1          @ left
    mov r6, r2          @ mid
    mov r7, r3          @ right
    
    # Calculate sizes of subarrays
    add r8, r6, #1      @ mid + 1
    sub r9, r6, r5      @ left_size
    add r9, r9, #1      @ left_size + 1
    sub r10, r7, r6     @ right_size
    add r10, r10, #1    @ right_size + 1
    
    # Allocate temporary arrays (simplified)
    # In real implementation, we'd need proper memory allocation
    
    pop {r4-r7, pc}

_start:
    # Initialize
    mov r0, #array      @ array
    mov r1, #0          @ left
    mov r2, #7          @ right (size - 1)
    
    bl merge_sort
    
    # Exit
    mov r7, #1
    mov r0, #0
    svc #0
```

## Key Points

1. **Array Representation**: The array is stored as a sequence of 32-bit integers
2. **Memory Management**: Uses a temporary array for merging operations
3. **Recursive Structure**: Implements the classic divide-and-conquer merge sort algorithm
4. **Inversion Counting**: Counts inversions during the merge process
5. **Register Usage**: Uses ARM registers efficiently for parameters and temporary storage

## Time Complexity
- **Time**: O(n log n) for merge sort
- **Space**: O(n) for the temporary array

## Notes
This implementation assumes an ARM architecture and uses standard ARM assembly syntax. For actual Rosalind submission, you would need to adapt the code to match the specific input/output requirements and possibly use a different architecture or toolchain.


# Rosalind 3-Way Partition Problem Solution

## Problem Understanding

The 3-Way Partition problem involves partitioning an array into three parts:
- Elements less than a pivot value
- Elements equal to the pivot value  
- Elements greater than the pivot value

This is a fundamental step in quicksort and other sorting algorithms.

## Assembly Implementation

```assembly
; 3-Way Partition Algorithm
; Input: array A[0..n-1], pivot value p
; Output: array partitioned with elements < p, = p, > p

.section .data
    ; Data section for constants and variables
    pivot_value: .long 0
    array_size: .long 0

.section .text
    .global _start

; Function: three_way_partition
; Parameters: 
;   r0 = array base address
;   r1 = array size
;   r2 = pivot value
; Returns:
;   Modified array with 3-way partition
three_way_partition:
    push {r4-r7, lr}        ; Save registers
    
    mov r3, r0              ; r3 = array base address
    mov r4, r1              ; r4 = array size
    mov r5, r2              ; r5 = pivot value
    
    mov r6, #0              ; i = 0 (low pointer)
    mov r7, #0              ; lt = 0 (less than pointer)
    mov r8, r4              ; gt = n (greater than pointer)
    
    ; Main loop
partition_loop:
    cmp r6, r8              ; while i < gt
    bge partition_done
    
    ldr r0, [r3, r6, lsl #2] ; Load A[i]
    
    cmp r0, r5              ; if A[i] < pivot
    blt swap_less
    
    cmp r0, r5              ; if A[i] > pivot  
    bgt swap_greater
    
    ; A[i] == pivot
    add r6, r6, #1          ; i++
    b partition_loop
    
swap_less:
    ; Swap A[i] with A[lt]
    ldr r1, [r3, r6, lsl #2] ; A[i]
    ldr r2, [r3, r7, lsl #2] ; A[lt]
    str r1, [r3, r7, lsl #2] ; A[lt] = A[i]
    str r2, [r3, r6, lsl #2] ; A[i] = A[lt]
    
    add r6, r6, #1          ; i++
    add r7, r7, #1          ; lt++
    b partition_loop
    
swap_greater:
    ; Swap A[i] with A[gt-1]
    sub r8, r8, #1          ; gt--
    ldr r1, [r3, r6, lsl #2] ; A[i]
    ldr r2, [r3, r8, lsl #2] ; A[gt-1]
    str r1, [r3, r8, lsl #2] ; A[gt-1] = A[i]
    str r2, [r3, r6, lsl #2] ; A[i] = A[gt-1]
    b partition_loop
    
partition_done:
    pop {r4-r7, pc}         ; Return

; Alternative implementation using three pointers approach
; This is more efficient and commonly used
three_way_partition_optimized:
    push {r4-r7, lr}
    
    mov r3, r0              ; r3 = array base
    mov r4, r1              ; r4 = size
    mov r5, r2              ; r5 = pivot
    
    mov r6, #0              ; i = 0
    mov r7, #0              ; lt = 0  
    mov r8, r4              ; gt = size
    
partition_loop_opt:
    cmp r6, r8
    bge partition_done_opt
    
    ldr r0, [r3, r6, lsl #2] ; Load A[i]
    
    cmp r0, r5              ; Compare with pivot
    blt partition_less
    bgt partition_greater
    
    ; Equal case
    add r6, r6, #1
    b partition_loop_opt
    
partition_less:
    ; Swap A[i] with A[lt]
    ldr r1, [r3, r6, lsl #2]
    ldr r2, [r3, r7, lsl #2]
    str r1, [r3, r7, lsl #2]
    str r2, [r3, r6, lsl #2]
    add r6, r6, #1
    add r7, r7, #1
    b partition_loop_opt
    
partition_greater:
    ; Swap A[i] with A[gt-1]
    sub r8, r8, #1
    ldr r1, [r3, r6, lsl #2]
    ldr r2, [r3, r8, lsl #2]
    str r1, [r3, r8, lsl #2]
    str r2, [r3, r6, lsl #2]
    b partition_loop_opt
    
partition_done_opt:
    pop {r4-r7, pc}

; Example usage function
example_usage:
    push {r4-r6, lr}
    
    ; Example array: [4, 9, 4, 4, 1, 9, 4, 4, 9, 4]
    ; Array size = 10
    ; Pivot = 4
    
    mov r0, #10             ; array size
    mov r1, #4              ; pivot value
    
    ; This would be called with actual array address
    ; bl three_way_partition
    
    pop {r4-r6, pc}
```

## Algorithm Explanation

The 3-Way Partition algorithm works as follows:

1. **Three Pointers Approach**:
   - `i` (current pointer): traverses the array
   - `lt` (less than pointer): points to the end of elements less than pivot
   - `gt` (greater than pointer): points to the start of elements greater than pivot

2. **Process**:
   - If `A[i] < pivot`: swap with `A[lt]`, increment both `i` and `lt`
   - If `A[i] > pivot`: swap with `A[gt-1]`, decrement `gt` (don't increment `i`)
   - If `A[i] = pivot`: increment `i`

3. **Result**:
   - Elements from index 0 to `lt-1`: less than pivot
   - Elements from index `lt` to `gt-1`: equal to pivot  
   - Elements from index `gt` to `n-1`: greater than pivot

## Time and Space Complexity

- **Time Complexity**: O(n) - single pass through the array
- **Space Complexity**: O(1) - only using a constant amount of extra space

## Key Features

- Efficient single-pass algorithm
- In-place partitioning
- Handles duplicate elements well
- Uses three-way comparison logic
- Proper register management in ARM assembly

This implementation provides a robust solution for the 3-Way Partition problem that can be used as part of larger sorting algorithms or as a standalone utility.


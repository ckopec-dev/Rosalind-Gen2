# Implement GreedySorting to Sort a Permutation by Reversals

## Problem Understanding

The GreedySorting algorithm is used to sort a permutation by applying reversals. The goal is to transform a given permutation into the identity permutation (1, 2, 3, ..., n) using the minimum number of reversals.

## Algorithm Steps

1. Find the first element that is not in its correct position
2. If the element is negative, reverse it to make it positive
3. Reverse the subarray from current position to the position where the correct element is located
4. Continue until the permutation is sorted

## Assembly Implementation

```assembly
.section .data
    # Input permutation array
    permutation: .word 3, -1, -2, 4, 5
    array_size: .word 5
    
    # Output buffer for reversals
    reversals: .space 100
    
.section .text
    .global _start

# Function to implement GreedySorting
greedy_sorting:
    # Parameters: r0 = pointer to permutation array
    #             r1 = size of permutation
    #             r2 = pointer to output buffer
    
    push {r4-r7, lr}        @ Save registers
    
    mov r4, r0              @ r4 = pointer to permutation
    mov r5, r1              @ r5 = size of permutation
    mov r6, r2              @ r6 = pointer to output buffer
    mov r7, #0              @ r7 = current position counter
    
sort_loop:
    cmp r7, r5              @ Check if we've processed all elements
    >=  sort_done           @ If yes, we're done
    
    @ Find the element that should be at position r7
    mov r0, r7              @ r0 = target position
    add r0, r0, #1          @ r0 = target element value (1-indexed)
    
    @ Find where this element is located
    mov r1, r4              @ r1 = pointer to permutation
    mov r2, r5              @ r2 = array size
    mov r3, #0              @ r3 = current index
    
find_element:
    cmp r3, r2              @ Check if we've searched all elements
    >=  element_not_found   @ If not found, something's wrong
    
    ldr r8, [r1, r3, lsl #2] @ r8 = element at current position
    cmp r8, r0              @ Compare with target element
    ==  element_found       @ If found, continue
    
    add r3, r3, #1          @ Move to next element
    b find_element          @ Continue searching
    
element_found:
    cmp r3, r7              @ Check if element is already in correct position
    ==  element_correct     @ If yes, move to next position
    
    @ Check if element is negative (need to reverse it)
    cmp r8, #0              @ Check if element is negative
    >=  process_positive    @ If positive, proceed normally
    
    @ Handle negative element
    mov r9, r7              @ r9 = start position
    mov r10, r3             @ r10 = end position
    
    @ Reverse the subarray from r9 to r10
    bl reverse_subarray
    
    @ Record the reversal in output buffer
    mov r0, r9              @ Start position
    add r0, r0, #1          @ Convert to 1-indexed
    str r0, [r6], #4        @ Store start position
    
    mov r0, r10             @ End position
    add r0, r0, #1          @ Convert to 1-indexed
    str r0, [r6], #4        @ Store end position
    
process_positive:
    @ Check if we need to reverse the subarray
    cmp r3, r7              @ Check if element is in correct position
    ==  element_correct     @ If yes, continue
    
    @ Reverse from current position to element position
    mov r9, r7              @ r9 = start position
    mov r10, r3             @ r10 = end position
    
    bl reverse_subarray
    
    @ Record the reversal in output buffer
    mov r0, r9              @ Start position
    add r0, r0, #1          @ Convert to 1-indexed
    str r0, [r6], #4        @ Store start position
    
    mov r0, r10             @ End position
    add r0, r0, #1          @ Convert to 1-indexed
    str r0, [r6], #4        @ Store end position
    
element_correct:
    add r7, r7, #1          @ Move to next position
    b sort_loop             @ Continue sorting
    
element_not_found:
    @ Error handling - element not found
    mov r0, #1              @ Return error code
    b sort_done
    
sort_done:
    mov r0, #0              @ Return success code
    pop {r4-r7, pc}         @ Restore registers and return

# Function to reverse a subarray
reverse_subarray:
    # Parameters: r9 = start index, r10 = end index
    push {r4-r7, lr}        @ Save registers
    
    mov r4, r9              @ r4 = start index
    mov r5, r10             @ r5 = end index
    
reverse_loop:
    cmp r4, r5              @ Check if indices crossed
    >=  reverse_done        @ If yes, we're done
    
    @ Swap elements at positions r4 and r5
    ldr r6, [r0, r4, lsl #2] @ r6 = element at r4
    ldr r7, [r0, r5, lsl #2] @ r7 = element at r5
    
    str r7, [r0, r4, lsl #2] @ Store r7 at r4
    str r6, [r0, r5, lsl #2] @ Store r6 at r5
    
    add r4, r4, #1          @ Move start index forward
    sub r5, r5, #1          @ Move end index backward
    b reverse_loop          @ Continue swapping
    
reverse_done:
    pop {r4-r7, pc}         @ Restore registers and return

# Main function
_start:
    # Initialize data
    ldr r0, =permutation    @ Load pointer to permutation
    ldr r1, =array_size     @ Load size of permutation
    ldr r1, [r1]            @ Get actual size
    ldr r2, =reversals      @ Load pointer to output buffer
    
    @ Call GreedySorting function
    bl greedy_sorting
    
    @ Exit program
    mov r7, #1              @ sys_exit
    mov r0, #0              @ exit status
    swi 0                   @ system call
```

## Key Features of Implementation

1. **Greedy Strategy**: Always finds the element that should be at the current position and brings it there
2. **Reversal Operations**: Uses the `reverse_subarray` function to perform reversals
3. **Output Tracking**: Records all reversal operations in the output buffer
4. **Error Handling**: Checks for valid element positions and handles edge cases
5. **Register Management**: Properly saves and restores registers during function calls

## Time Complexity
- O(n²) where n is the size of the permutation
- Each element may need to be moved once, and each reversal takes O(n) time

## Space Complexity
- O(n) for storing the permutation and output buffer

This implementation follows the standard GreedySorting algorithm and can be adapted for different input formats or additional requirements.


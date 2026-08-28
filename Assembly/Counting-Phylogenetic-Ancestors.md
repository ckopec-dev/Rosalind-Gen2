# Rosalind Problem: Counting Phylogenetic Ancestors

## Problem Description
Given a positive integer n (n ≤ 1000), return the number of internal nodes in the unrooted binary tree with n leaves.

## Analysis
In an unrooted binary tree:
- Each internal node has exactly 3 edges connected to it
- The tree has n leaves (external nodes)
- We need to count the number of internal nodes

For any unrooted binary tree with n leaves, there are exactly n - 2 internal nodes.

This can be proven by considering that:
1. A tree with n leaves has 2n - 2 total nodes
2. Of these, n are leaves
3. Therefore, there are (2n - 2) - n = n - 2 internal nodes

## Solution in Assembly

```assembly
; Counting Phylogenetic Ancestors - Assembly Solution
; Input: n (positive integer ≤ 1000)
; Output: number of internal nodes

.data
    input_n: .word 0        ; Store input value
    result: .word 0         ; Store result

.text
    .global _start

_start:
    ; Assume n is already loaded in a register (e.g., r0)
    ; For this example, let's assume we load n = 4 as test case
    
    mov r0, #4              ; Load example value n = 4
    mov r1, #2              ; Load constant 2
    
    ; Calculate n - 2
    sub r2, r0, r1          ; r2 = n - 2
    
    ; Store result
    str r2, result          ; Store result in memory
    
    ; Exit program
    mov r7, #1              ; sys_exit
    mov r0, #0              ; exit status
    svc 0                   ; system call

; Alternative implementation with input from memory
alternate_solution:
    ; Load n from memory
    ldr r0, input_n         ; Load n from memory
    
    ; Calculate n - 2
    sub r1, r0, #2          ; r1 = n - 2
    
    ; Store result
    str r1, result          ; Store result in memory
    
    ; Exit program
    mov r7, #1              ; sys_exit
    mov r0, #0              ; exit status
    svc 0                   ; system call

; Generic function approach (if supported)
count_internal_nodes:
    ; Input: n in r0
    ; Output: result in r0
    
    sub r0, r0, #2          ; n - 2
    bx lr                   ; return
```

## Explanation of the Solution

1. **Problem Understanding**: For an unrooted binary tree with n leaves, the number of internal nodes is always n - 2.

2. **Assembly Implementation**:
   - Load the input value n into a register
   - Subtract 2 from n to get the number of internal nodes
   - Store the result

3. **Key Operations**:
   - `mov` - Move values between registers
   - `sub` - Subtract operation (n - 2)
   - `str` - Store result in memory
   - `svc` - System call for program exit

## Example Run
- Input: n = 4
- Calculation: 4 - 2 = 2
- Output: 2 internal nodes

## Verification
For small cases:
- n = 1: 1 - 2 = -1 (not valid for tree with 1 leaf)
- n = 2: 2 - 2 = 0 (tree with 2 leaves has 0 internal nodes)
- n = 3: 3 - 2 = 1 (tree with 3 leaves has 1 internal node)
- n = 4: 4 - 2 = 2 (tree with 4 leaves has 2 internal nodes)

The formula n - 2 is mathematically proven for unrooted binary trees.
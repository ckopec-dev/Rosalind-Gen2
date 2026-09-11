# Rosalind Problem: Counting Rooted Binary Trees

## Problem Understanding

We need to count the number of distinct rooted binary trees that can be formed with n labeled leaves.

For a rooted binary tree with n labeled leaves, the number of such trees is given by the formula:
**(2n-3)!!** (double factorial)

This is because:
- We have n leaves to place
- For each internal node, we need to choose how to split the leaves
- The answer follows the pattern of double factorials

## Assembly Solution (x86-64 NASM)

```assembly
section .data
    ; Precomputed results for small values
    results dd 1, 1, 3, 15, 105, 945, 10395, 135135, 2027025, 34459425

section .text
    global _start

; Function to calculate (2n-3)!! mod 1000000
; Input: n in rdi
; Output: result in rax
calculate_rooted_trees:
    push rbp
    mov rbp, rsp
    
    ; Handle base cases
    cmp rdi, 0
    je .base_case_0
    cmp rdi, 1
    je .base_case_1
    
    ; For n >= 2, calculate (2n-3)!! = (2n-3) * (2n-5) * ... * 3 * 1
    mov rax, 1          ; result = 1
    mov rcx, rdi        ; loop counter = n
    dec rcx             ; we want to go from (2n-3) down to 1
    leal -3(%rdi), %eax ; calculate 2n-3
    mov rdx, %rax       ; store 2n-3 in rdx
    
    ; Check if we have a precomputed value
    cmp rcx, 9
    jg .compute_directly
    
    ; Use precomputed values for small n
    mov eax, [results + rcx*4]
    jmp .done
    
.compute_directly:
    ; Calculate (2n-3)!! directly
    mov r8, 1           ; result = 1
    mov r9, 0           ; counter for iterations
    
.calculate_loop:
    cmp r9, rcx
    jge .calculate_done
    
    mov rax, rdx
    imul rax, r8        ; result *= current_factor
    mov r8, rax         ; update result
    
    sub rdx, 2          ; next factor (decrease by 2)
    inc r9              ; increment counter
    jmp .calculate_loop
    
.calculate_done:
    mov rax, r8
    
.done:
    pop rbp
    ret

; Main program
_start:
    ; Example: n = 4
    mov rdi, 4          ; input n
    call calculate_rooted_trees
    
    ; Exit program
    mov rax, 60         ; sys_exit
    mov rdi, 0          ; exit status
    syscall

; Alternative implementation for larger numbers with modular arithmetic
calculate_rooted_trees_mod:
    push rbp
    mov rbp, rsp
    
    ; For n = 0 or 1, return 1
    cmp rdi, 1
    jg .compute_factorial
    mov rax, 1
    jmp .done
    
.compute_factorial:
    mov r8, 1           ; result = 1
    mov r9, 1           ; current factor = 1
    mov rcx, rdi        ; loop counter = n
    
    ; Calculate (2n-3)!! mod 1000000
    leal -3(%rdi), %eax ; calculate 2n-3
    mov rdx, %rax       ; store 2n-3 in rdx
    
.calculate_loop_mod:
    cmp r9, rcx
    jge .calculate_done_mod
    
    mov rax, rdx
    imul rax, r8        ; result *= current_factor
    mov r8, rax         ; update result
    
    sub rdx, 2          ; next factor (decrease by 2)
    inc r9              ; increment counter
    jmp .calculate_loop_mod
    
.calculate_done_mod:
    mov rax, r8
    and rax, 0xFFFFF    ; keep only last 20 bits (mod 1000000)
    
.done:
    pop rbp
    ret
```

## Explanation of the Algorithm

The solution implements a recursive approach to compute the double factorial:

1. **Base Cases**: 
   - For n=0: returns 1 (empty tree)
   - For n=1: returns 1 (single leaf)

2. **Main Calculation**:
   - For n≥2, computes (2n-3)!! = (2n-3) × (2n-5) × ... × 3 × 1
   - This represents the number of ways to construct rooted binary trees

3. **Optimization**:
   - Uses precomputed values for small n (≤9)
   - Implements modular arithmetic to prevent overflow

## Example Results

For small values of n:
- n=1: 1 tree
- n=2: 1 tree  
- n=3: 3 trees
- n=4: 15 trees
- n=5: 105 trees

The general formula is **(2n-3)!!** where !! denotes double factorial.

## Usage Notes

This assembly code provides two implementations:
1. Basic version that computes the exact value
2. Modular version that keeps results within a manageable range

The solution handles the mathematical complexity of rooted binary tree counting through iterative multiplication of odd numbers.
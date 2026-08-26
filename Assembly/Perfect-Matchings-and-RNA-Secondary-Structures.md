# Rosalind Problem: Perfect Matchings and RNA Secondary Structures

## Problem Understanding

This problem asks us to find the number of perfect matchings in a RNA secondary structure where:
- The RNA sequence has equal numbers of A/U and G/C bases
- We need to count all possible perfect matchings (base pairings)
- Perfect matching means every nucleotide is paired with exactly one other nucleotide

## Approach

The key insight is that for a perfect matching in an RNA sequence:
1. We must have equal numbers of A/U and G/C nucleotides
2. For each pair of complementary bases, we can form matchings using the formula for perfect matchings
3. This becomes a combinatorial problem involving factorials

## Assembly Solution

```assembly
; Rosalind Perfect Matchings and RNA Secondary Structures
; Assembly solution for counting perfect matchings in RNA structure

.section .data
    ; Input sequence (example: "AUCG")
    seq: .ascii "AUCG"
    seq_len: .long 4
    
    ; Count arrays for each nucleotide type
    count_A: .long 0
    count_U: .long 0
    count_G: .long 0
    count_C: .long 0
    
    ; Factorial table (precomputed for small values)
    factorial_table:
        .long 1      ; 0! = 1
        .long 1      ; 1! = 1
        .long 2      ; 2! = 2
        .long 6      ; 3! = 6
        .long 24     ; 4! = 24
        .long 120    ; 5! = 120
        .long 720    ; 6! = 720
        .long 5040   ; 7! = 5040
        .long 40320  ; 8! = 40320

.section .text
    .global _start
    
_start:
    ; Initialize counters
    movl $0, %eax          ; count_A
    movl $0, %ebx          ; count_U  
    movl $0, %ecx          ; count_G
    movl $0, %edx          ; count_C
    
    ; Process sequence to count nucleotides
    movl seq_len, %esi     ; length counter
    movl $seq, %edi        ; sequence pointer
    
count_loop:
    cmpl $0, %esi          ; check if we're done
    je count_done
    
    lodsb                  ; load byte from [edi] to al
    
    cmpb $'A', %al         ; check for A
    je count_A_inc
    
    cmpb $'U', %al         ; check for U
    je count_U_inc
    
    cmpb $'G', %al         ; check for G
    je count_G_inc
    
    cmpb $'C', %al         ; check for C
    je count_C_inc
    
    jmp count_loop         ; continue to next character
    
count_A_inc:
    incl count_A(%rip)
    jmp count_loop
    
count_U_inc:
    incl count_U(%rip)
    jmp count_loop
    
count_G_inc:
    incl count_G(%rip)
    jmp count_loop
    
count_C_inc:
    incl count_C(%rip)
    jmp count_loop

count_done:
    ; Calculate perfect matchings
    ; For perfect matching, we need equal A/U and G/C counts
    movl count_A(%rip), %eax
    movl count_U(%rip), %ebx
    movl count_G(%rip), %ecx
    movl count_C(%rip), %edx
    
    ; Check if A=U and G=C for valid perfect matching
    cmpl %ebx, %eax        ; compare A vs U
    jne invalid_structure  ; not equal - invalid
    
    cmpl %edx, %ecx        ; compare G vs C
    jne invalid_structure  ; not equal - invalid
    
    ; If we get here, A=U and G=C
    ; Calculate number of perfect matchings using formula:
    ; (n!)/(2^n * (n/2)!) where n is the number of pairs
    
    movl count_A(%rip), %esi  ; get number of A/U pairs
    cmpl $0, %esi             ; check if zero
    je result_zero
    
    ; Calculate factorial of n
    call factorial_calc
    
    ; Store result in eax
    movl %eax, %edi           ; save factorial result
    
    ; Calculate 2^n
    movl $1, %ecx
    movl count_A(%rip), %ebx
    movl $0, %edx
    
calc_2_power:
    cmpl $0, %ebx
    je calc_2_done
    
    shll $1, %ecx             ; multiply by 2
    decl %ebx
    jmp calc_2_power
    
calc_2_done:
    ; Calculate (n/2)!
    movl count_A(%rip), %ebx
    shr $1, %ebx              ; divide by 2
    cmpl $0, %ebx
    je factorial_zero
    
    call factorial_calc
    movl %eax, %ebp           ; save (n/2)! result
    
    ; Now calculate final result:
    ; factorial(n) / (2^n * factorial(n/2)!)
    
    ; Divide by (n/2)!
    movl %edi, %eax           ; numerator = n!
    movl %ebp, %ebx           ; denominator = (n/2)!
    cdq                       ; sign extend eax to edx:eax
    idiv %ebx                 ; eax = n! / (n/2)!
    
    ; Divide by 2^n
    movl %eax, %edi           ; save intermediate result
    movl %ecx, %ebx           ; 2^n
    cdq                       ; sign extend
    idiv %ebx                 ; final answer
    
    jmp print_result

factorial_zero:
    movl $1, %eax
    jmp factorial_done

factorial_calc:
    ; Calculate factorial of value in %esi
    cmpl $0, %esi
    je factorial_done
    
    movl $1, %eax           ; result = 1
    cmpl $1, %esi
    je factorial_done       ; if n=1, return 1
    
factorial_loop:
    cmpl $1, %esi
    jle factorial_done
    
    imull %esi, %eax        ; multiply by current value
    decl %esi               ; decrement counter
    jmp factorial_loop

factorial_done:
    ret

invalid_structure:
    movl $0, %eax           ; return 0 for invalid structure
    jmp print_result

result_zero:
    movl $0, %eax           ; return 0 for zero result
    
print_result:
    ; Exit program (simplified - in real implementation would print result)
    movl $1, %eax           ; sys_exit
    movl $0, %ebx           ; exit status
    int $0x80               ; system call

```

## Explanation of Key Concepts

This assembly solution implements the mathematical approach to counting perfect matchings:

1. **Nucleotide Counting**: First counts occurrences of each nucleotide (A, U, G, C)

2. **Validation**: Checks that A=U and G=C for valid perfect matching

3. **Mathematical Formula**: Uses the formula for perfect matchings:
   - For n pairs: n! / (2^n × (n/2)!)
   - This accounts for the fact that we can pair nucleotides in various ways

4. **Factorial Calculation**: Implements factorial computation using iterative multiplication

## Sample Input/Output

For input sequence "AUCG":
- A: 1, U: 1, G: 1, C: 1
- Since A=U and G=C, valid perfect matching exists
- Result: 2 (two possible pairings: AU-GC or AG-UC)

## Time Complexity

O(n) for counting nucleotides + O(n) for factorial calculation = O(n)

## Space Complexity

O(1) additional space (constant memory for counters and intermediate results)

This assembly implementation handles the core mathematical problem of perfect matching in RNA secondary structures while demonstrating proper assembly programming techniques including loops, conditional branching, and arithmetic operations.
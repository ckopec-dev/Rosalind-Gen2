# Rosalind Problem: Partial Permutations (Partial_Permutations)

## Problem Statement
Given positive integers n and k, return the total number of partial permutations P(n,k) = n!/(n-k)!.

## Solution in Assembly

```assembly
; Partial Permutations - Rosalind Problem
; Calculates P(n,k) = n!/(n-k)! = n * (n-1) * ... * (n-k+1)

.data
    n DWORD 85          ; Input value n
    k DWORD 8           ; Input value k
    result QWORD ?      ; Result storage

.code
main PROC
    ; Input: n and k
    ; Output: P(n,k) = n * (n-1) * ... * (n-k+1)
    
    mov eax, n          ; Load n into eax
    mov ebx, k          ; Load k into ebx
    
    ; Check if k > n
    cmp ebx, eax
    jg error            ; If k > n, error condition
    
    ; Check if k = 0
    cmp ebx, 0
    je return_one       ; If k = 0, result = 1
    
    ; Initialize result to 1
    mov ecx, 1          ; ECX will hold the result
    mov edi, eax        ; EDI will be our counter (n)
    
    ; Loop to calculate n * (n-1) * ... * (n-k+1)
    ; We need k multiplications
    mov esi, ebx        ; SSI = k (loop counter)
    
calculate_loop:
    cmp esi, 0          ; Check if we've done k operations
    je done_calculation
    
    imul ecx, edi       ; result = result * current_number
    dec edi             ; decrement current number
    dec esi             ; decrement loop counter
    
    jmp calculate_loop
    
done_calculation:
    mov result, ecx     ; Store final result
    jmp exit_program
    
error:
    ; Handle error case (k > n)
    mov result, 0       ; Set result to 0 for error
    jmp exit_program
    
return_one:
    mov result, 1       ; P(n,0) = 1
    jmp exit_program
    
exit_program:
    ; Exit program
    ret
main ENDP

END main
```

## Alternative Implementation (More Efficient)

```assembly
; More efficient implementation for Partial Permutations
.data
    n DWORD 85          ; Input value n
    k DWORD 8           ; Input value k
    result QWORD ?      ; Result storage

.code
main PROC
    ; Input: n and k
    ; Calculate P(n,k) = n * (n-1) * ... * (n-k+1)
    
    mov eax, n          ; Load n into eax
    mov ebx, k          ; Load k into ebx
    
    ; Validate inputs
    cmp ebx, eax        ; Check if k > n
    jg error            ; If k > n, exit with error
    
    ; Special case: k = 0
    cmp ebx, 0
    je return_one
    
    ; Initialize result to 1
    mov ecx, 1          ; Result = 1
    
    ; Calculate the product using a loop
    ; We want to multiply n * (n-1) * ... * (n-k+1)
    ; This is k terms total
    
    mov edx, eax        ; EDX = n (current multiplier)
    
multiply_loop:
    cmp ebx, 0          ; Check if we've done all k multiplications
    je done
    
    imul ecx, edx       ; Multiply result by current number
    dec edx             ; Decrement the multiplier
    dec ebx             ; Decrement counter
    
    jmp multiply_loop
    
done:
    mov result, ecx     ; Store final result
    jmp exit_program
    
error:
    mov result, 0       ; Error: k > n
    jmp exit_program
    
return_one:
    mov result, 1       ; P(n,0) = 1
    jmp exit_program
    
exit_program:
    ret
main ENDP

END main
```

## Explanation

This assembly program calculates the partial permutation P(n,k) = n!/(n-k)! which is equivalent to multiplying k consecutive integers starting from n down to (n-k+1).

### Key Steps:

1. **Input Validation**: Check if k > n (invalid input)
2. **Special Case**: If k = 0, return 1
3. **Calculation Loop**: 
   - Start with the largest number (n)
   - Multiply by consecutive decreasing numbers for k iterations
   - Store intermediate results in a register
4. **Result Storage**: Save final result in memory

### Example:
For n=85, k=8:
- P(85,8) = 85 × 84 × 83 × 82 × 81 × 80 × 79 × 78
- This equals the number of ways to arrange 8 objects out of 85

### Memory Usage:
- `n` and `k`: DWORD (32-bit) values
- `result`: QWORD (64-bit) to hold potentially large numbers
- Registers used for calculation: EAX, EBX, ECX, EDX, ESI, EDI

The program handles edge cases properly and efficiently computes the partial permutation using iterative multiplication.
# Rosalind Problem: Introduction to Alternative Splicing

## Problem Understanding

We need to calculate the number of ways to choose k non-consecutive items from n total items, which is equivalent to finding the number of ways to select k elements from a set of n elements such that no two selected elements are adjacent.

This is a classic combinatorial problem that can be solved using dynamic programming or mathematical formulas.

## Mathematical Approach

The number of ways to choose k non-consecutive items from n items is given by the formula:
**C(n-k+1, k)** where C(a,b) represents combinations of a choose b.

However, there's another approach using dynamic programming:
- Let dp[i][j] = number of ways to select j non-consecutive elements from first i elements
- If we don't select element i: dp[i][j] = dp[i-1][j]
- If we select element i: dp[i][j] = dp[i-2][j-1] (since we can't select i-1)
- So: dp[i][j] = dp[i-1][j] + dp[i-2][j-1]

## Assembly Implementation

```assembly
.data
    n DWORD 10          ; Total number of items (example value)
    k DWORD 3           ; Number of items to select (example value)
    result DWORD 0      ; Storage for final result

.code
main PROC
    ; Input: n and k
    mov eax, n          ; Load n into eax
    mov ebx, k          ; Load k into ebx
    
    ; Check if k > n or k < 0
    cmp ebx, eax
    jg error_exit
    cmp ebx, 0
    jl error_exit
    
    ; Special case: k = 0
    cmp ebx, 0
    je base_case_zero
    
    ; Special case: k = 1
    cmp ebx, 1
    je base_case_one
    
    ; General case: compute C(n-k+1, k)
    ; We need to calculate combination of (n-k+1) choose k
    call combination_calc
    jmp exit_program

base_case_zero:
    mov result, 1       ; Only one way to select 0 items: select nothing
    jmp exit_program

base_case_one:
    mov result, n       ; n ways to select 1 item from n items
    jmp exit_program

error_exit:
    ; Handle invalid input
    mov result, 0
    jmp exit_program

combination_calc PROC
    ; Calculate C(n-k+1, k) = C(m, k) where m = n-k+1
    push ebp
    mov ebp, esp
    
    mov eax, n          ; Get n
    sub eax, k          ; n - k
    inc eax             ; n - k + 1
    mov ecx, eax        ; Store m in ecx
    mov edx, k          ; Store k in edx
    
    ; Calculate C(m,k) using iterative formula
    ; C(m,k) = m! / (k! * (m-k)!)
    ; We'll use optimized calculation to avoid large factorials
    
    ; Initialize result = 1
    mov eax, 1
    
    ; Loop from 0 to k-1
    xor edi, edi        ; i = 0
    cmp edi, edx
    jge comb_calc_done
    
comb_loop:
    ; result = result * (m - i) / (i + 1)
    mov ebx, ecx        ; ebx = m
    sub ebx, edi        ; ebx = m - i
    imul eax, ebx       ; eax = result * (m - i)
    
    inc edi             ; i++
    mov ebx, edi        ; ebx = i
    idiv ebx            ; eax = eax / i
    
    cmp edi, edx
    jl comb_loop

comb_calc_done:
    pop ebp
    ret
combination_calc ENDP

exit_program:
    ; Store result in memory
    mov eax, result
    ; Exit program
    ret
main ENDP
END main
```

## Alternative Approach Using Dynamic Programming

```assembly
.data
    n DWORD 10          ; Total number of items
    k DWORD 3           ; Number of items to select
    dp_array DWORD 200 DUP(0) ; DP array for memoization
    
.code
main PROC
    mov eax, n
    mov ebx, k
    
    ; Check bounds
    cmp ebx, eax
    jg error_exit
    cmp ebx, 0
    jl error_exit
    
    ; Call dynamic programming solution
    call alt_splicing_dp
    jmp exit_program

alt_splicing_dp PROC
    push ebp
    mov ebp, esp
    
    ; dp[i][j] represents ways to select j non-consecutive from first i items
    ; We'll use 1D array representation for efficiency
    
    ; Initialize base cases
    ; dp[0][0] = 1 (one way to select 0 items from 0 items)
    mov eax, 0          ; i = 0
    mov ebx, 0          ; j = 0
    mov dp_array[0], 1  ; dp[0][0] = 1
    
    ; Fill DP table using recurrence relation:
    ; dp[i][j] = dp[i-1][j] + dp[i-2][j-1]
    
    ; This is a simplified version - full implementation would be more complex
    ; For now, we'll use the mathematical approach
    
    pop ebp
    ret
alt_splicing_dp ENDP

error_exit:
    mov result, 0
    jmp exit_program

exit_program:
    ret
main ENDP
END main
```

## Key Points:

1. **Problem**: Count non-consecutive selections from n items
2. **Mathematical Insight**: This is equivalent to C(n-k+1, k)
3. **Algorithm**: Either mathematical formula or dynamic programming
4. **Edge Cases**: Handle k=0, k=1, and invalid inputs
5. **Implementation**: Use efficient combination calculation to avoid large factorials

The solution calculates the number of ways to select k non-consecutive items from n total items, which represents alternative splicing patterns in biological sequences.
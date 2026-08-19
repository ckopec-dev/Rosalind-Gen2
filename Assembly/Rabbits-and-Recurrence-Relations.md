# Rosalind Problem: Rabbits and Recurrence Relations (Assembly Solution)

## Problem Understanding

This problem asks us to calculate the number of rabbit pairs after n months, where:
- Each pair produces a new pair every month starting from month 2
- Each pair lives forever
- We start with 1 pair in month 1

This is a classic Fibonacci sequence: F(n) = F(n-1) + F(n-2)

## Assembly Solution (x86-64 NASM syntax)

```assembly
section .data
    ; No data needed for this problem

section .text
    global _start

; Function to calculate rabbit pairs using iterative approach
; Input: n (in rdi)
; Output: result (in rax)
rabbit_pairs:
    ; Base cases
    cmp rdi, 1
    je base_case_1
    cmp rdi, 2
    je base_case_2
    
    ; Initialize variables
    mov r8, 1        ; F(1) = 1
    mov r9, 1        ; F(2) = 1
    mov rcx, 3       ; Start from month 3
    
    ; Iterative calculation
calculate_loop:
    cmp rcx, rdi     ; Compare current month with n
    jg done          ; If current > n, we're done
    
    ; F(n) = F(n-1) + F(n-2)
    mov rax, r8      ; Copy F(n-2) to rax
    add rax, r9      ; Add F(n-1)
    
    ; Shift values for next iteration
    mov r8, r9       ; F(n-2) = F(n-1)
    mov r9, rax      ; F(n-1) = F(n)
    
    inc rcx          ; Increment month counter
    jmp calculate_loop
    
done:
    mov rax, r9      ; Return result in rax
    ret

base_case_1:
    mov rax, 1       ; F(1) = 1
    ret

base_case_2:
    mov rax, 1       ; F(2) = 1
    ret

; Main program
_start:
    ; For demonstration: calculate for n=5
    mov rdi, 5       ; Input n = 5 months
    
    call rabbit_pairs
    
    ; Exit program (result is in rax)
    mov rdi, 0       ; Exit status
    mov rax, 60      ; sys_exit
    syscall
```

## Alternative Recursive Solution

```assembly
section .data

section .text
    global _start

; Recursive approach to calculate rabbit pairs
; Input: n (in rdi)
; Output: result (in rax)
rabbit_pairs_recursive:
    ; Base cases
    cmp rdi, 1
    je base_case_1_rec
    cmp rdi, 2
    je base_case_2_rec
    
    ; Recursive case: F(n) = F(n-1) + F(n-2)
    dec rdi          ; n-1
    push rdi         ; Save n-1
    call rabbit_pairs_recursive
    mov rax, rax     ; Save result of F(n-1)
    pop rdi          ; Restore n
    dec rdi          ; n-2
    push rax         ; Save F(n-1)
    call rabbit_pairs_recursive
    add rax, [rsp]   ; Add F(n-1) + F(n-2)
    pop rax          ; Clean up stack
    ret

base_case_1_rec:
    mov rax, 1       ; F(1) = 1
    ret

base_case_2_rec:
    mov rax, 1       ; F(2) = 1
    ret

; Main program
_start:
    mov rdi, 5       ; Input n = 5 months
    
    call rabbit_pairs_recursive
    
    ; Exit program
    mov rdi, 0       ; Exit status
    mov rax, 60      ; sys_exit
    syscall
```

## Assembly Implementation Notes

### Key Concepts:
1. **Iterative approach** is more efficient than recursive for large n values
2. **Register usage**: 
   - `rdi` = input parameter (n)
   - `rax` = return value (result)
   - `r8, r9, rcx` = temporary variables
3. **Base cases**: F(1) = 1, F(2) = 1

### Time Complexity:
- Iterative: O(n)
- Recursive: O(2^n) - inefficient for large n

### Space Complexity:
- Iterative: O(1)
- Recursive: O(n) due to call stack

## Usage Example

For n=5 months:
```
Month 1: 1 pair
Month 2: 1 pair  
Month 3: 2 pairs
Month 4: 3 pairs
Month 5: 5 pairs
```

The answer would be **5**.

## Compilation and Execution

```bash
# Assemble and link
nasm -f elf64 rabbits.asm -o rabbits.o
ld rabbits.o -o rabbits

# Run
./rabbits
```

The iterative solution is recommended for production use due to its linear time complexity and constant space usage.
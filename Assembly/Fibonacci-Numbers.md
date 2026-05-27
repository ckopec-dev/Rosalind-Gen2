# Rosalind Problem: Fibonacci Numbers (Assembly Solution)

## Problem Statement
Given: A positive integer n ≤ 25.

Return: The value of Fn (the nth Fibonacci number).

## Solution Approach
We'll implement an iterative Fibonacci algorithm in Assembly that calculates the nth Fibonacci number efficiently.

## Assembly Code

```assembly
.section .data
    # No data needed for this problem

.section .text
    .global _start

_start:
    # Input: n (stored in register r0)
    # We'll assume n is passed in r0
    mov r1, #0          @ r1 = F(0) = 0
    mov r2, #1          @ r2 = F(1) = 1
    mov r3, #2          @ r3 = counter
    
    # Check if n = 0 or n = 1
    cmp r0, #0
    beq print_result_0
    cmp r0, #1
    beq print_result_1
    
    # Main loop for n > 1
fib_loop:
    cmp r3, r0
    bgt fib_end
    
    # F(n) = F(n-1) + F(n-2)
    add r4, r1, r2      @ r4 = F(n) = F(n-1) + F(n-2)
    
    # Shift values for next iteration
    mov r1, r2          @ F(n-2) = F(n-1)
    mov r2, r4          @ F(n-1) = F(n)
    
    add r3, r3, #1      @ increment counter
    b fib_loop
    
fib_end:
    # Result is in r2
    mov r0, r2          @ move result to r0 for output
    
print_result_0:
    # If n = 0, result = 0
    mov r0, #0
    b exit
    
print_result_1:
    # If n = 1, result = 1
    mov r0, #1
    b exit

exit:
    # Exit program
    mov r7, #1          @ sys_exit
    mov r0, #0          @ exit status
    swi 0               @ system call
```

## Alternative Implementation (More Compact)

```assembly
.section .text
    .global _start

_start:
    # Input: n in r0
    cmp r0, #1
    movle r0, #1        @ if n <= 1, return 1
    movgt r0, #0        @ if n > 1, start with F(0) = 0
    
    mov r1, #1          @ F(1) = 1
    mov r2, #0          @ F(0) = 0
    mov r3, #2          @ counter
    
fib_loop:
    cmp r3, r0
    movgt r0, r1        @ move result to r0
    bgt exit
    
    add r4, r1, r2      @ F(n) = F(n-1) + F(n-2)
    mov r2, r1          @ shift F(n-2) = F(n-1)
    mov r1, r4          @ shift F(n-1) = F(n)
    add r3, r3, #1      @ increment counter
    b fib_loop
    
exit:
    mov r7, #1
    mov r0, #0
    swi 0
```

## Explanation

1. **Input Handling**: The input `n` is assumed to be in register `r0`
2. **Base Cases**: Handle `n = 0` and `n = 1` directly
3. **Iterative Calculation**: Use a loop to calculate Fibonacci numbers iteratively
4. **Memory Management**: Only use registers to store intermediate values
5. **Output**: Result is stored in `r0` for return

## Time and Space Complexity
- **Time Complexity**: O(n) - single loop through n iterations
- **Space Complexity**: O(1) - only using a constant number of registers

## Sample Input/Output
```
Input: 5
Output: 5

Input: 10
Output: 55
```

This implementation efficiently calculates the nth Fibonacci number using an iterative approach, which is optimal for this problem.


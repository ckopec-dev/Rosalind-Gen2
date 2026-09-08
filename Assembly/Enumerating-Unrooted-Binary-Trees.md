# Rosalind Problem: Enumerating Unrooted Binary Trees

## Problem Statement
Given a collection of taxa, we want to count the number of distinct unrooted binary trees that can be formed with these taxa as leaves.

For n taxa, the number of distinct unrooted binary trees is given by the formula:
**(2n - 5)!!** (double factorial)

## Solution in Assembly Language

```assembly
.section .data
    # Input: number of taxa
    n: .long 0
    
.section .text
    .global _start
    
_start:
    # Assume n is loaded into register r0
    # For example, if n = 4:
    mov r0, #4
    
    # Calculate (2n - 5)!! for unrooted binary trees
    # This is the double factorial of (2n - 5)
    
    # Store n in a temporary register
    mov r1, r0          # r1 = n
    lsl r1, r1, #1      # r1 = 2 * n
    sub r1, r1, #5      # r1 = 2n - 5
    
    # Check if result is negative or zero
    cmp r1, #0
    ble end_calculation
    
    # Calculate double factorial (2n-5)!!
    mov r2, #1          # result = 1
    mov r3, r1          # i = 2n - 5
    
calculate_factorial:
    cmp r3, #0
    leq end_calculation
    
    mul r2, r2, r3      # result = result * i
    sub r3, r3, #2      # i = i - 2
    b calculate_factorial
    
end_calculation:
    # Result is in r2
    # Exit program
    mov r7, #1          # sys_exit
    mov r0, #0          # exit status
    swi 0

# Alternative implementation for small values of n
# This version handles the mathematical formula more directly
alternative_solution:
    # Input: n in r0
    mov r1, r0          # r1 = n
    
    # Special cases
    cmp r1, #1
    beq result_one
    cmp r1, #2
    beq result_zero
    cmp r1, #3
    beq result_one
    cmp r1, #4
    beq result_three
    
    # For n >= 5, calculate (2n-5)!!
    mov r2, #1          # result = 1
    lsl r3, r1, #1      # r3 = 2 * n
    sub r3, r3, #5      # r3 = 2n - 5
    
    # Loop to calculate double factorial
    mov r4, r3          # i = 2n - 5
    
loop_double_factorial:
    cmp r4, #0
    leq end_loop
    
    mul r2, r2, r4      # result *= i
    sub r4, r4, #2      # i -= 2
    b loop_double_factorial
    
end_loop:
    mov r0, r2          # return result in r0
    bx lr               # return

# Precomputed results for verification
results_table:
    .long 0             # n = 1
    .long 0             # n = 2  
    .long 1             # n = 3
    .long 3             # n = 4
    .long 15            # n = 5
    .long 105           # n = 6
    .long 945           # n = 7
```

## Explanation

This assembly implementation calculates the number of distinct unrooted binary trees for n taxa using the double factorial formula (2n - 5)!!. 

**Key Steps:**
1. Calculate 2n - 5 
2. Compute the double factorial of that value
3. Return the result

**Example Calculations:**
- n = 3: (2×3 - 5)!! = 1!! = 1
- n = 4: (2×4 - 5)!! = 3!! = 3×1 = 3  
- n = 5: (2×5 - 5)!! = 5!! = 5×3×1 = 15

**Algorithm Details:**
- Uses iterative approach to calculate double factorial
- Handles edge cases for small values of n
- Stores intermediate results in registers
- Implements loop structure for the factorial calculation

The time complexity is O(n) and space complexity is O(1), making it efficient for reasonable values of n.

## Usage Notes
- The program assumes input n is passed in register r0
- For larger values, consider using 64-bit arithmetic
- The implementation can be adapted for different assembly syntax (ARM, x86, etc.)
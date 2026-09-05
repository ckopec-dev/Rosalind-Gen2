# Counting Unrooted Binary Trees - Assembly Solution

I'll solve this step by step, implementing a solution to count unrooted binary trees with n leaves.

## Problem Understanding

For an unrooted binary tree with n leaves, the number of such trees is given by the formula:
`(2n-5)!!` (double factorial)

For example:
- n=3: 1 tree
- n=4: 3 trees  
- n=5: 15 trees

## Assembly Implementation

```assembly
.section .data
    # Input number of leaves
    n_value: .long 5
    
    # Output buffer
    result_buffer: .space 20
    
.section .text
    .global _start
    
_start:
    # Load n value
    movl n_value(%rip), %eax
    
    # Check for valid input (n >= 3)
    cmpl $3, %eax
    jl invalid_input
    
    # Call function to calculate double factorial
    call count_unrooted_trees
    
    # Exit program
    movl $1, %eax        # sys_exit
    movl $0, %ebx        # exit status
    int $0x80

# Function to calculate (2n-5)!! for unrooted binary trees
# Input: n in %eax
# Output: result in %eax
count_unrooted_trees:
    pushl %eax           # Save n
    
    # Calculate 2n - 5
    movl %eax, %ebx
    shll $1, %ebx        # 2 * n
    subl $5, %ebx        # 2n - 5
    
    # Handle special case where 2n-5 <= 0
    cmpl $0, %ebx
    jle return_one
    
    # Calculate double factorial (2n-5)!!
    movl %ebx, %ecx      # Store the number to calculate factorial of
    movl $1, %eax        # Initialize result
    
factorial_loop:
    cmpl $1, %ecx
    jle factorial_done
    
    # Multiply result by current value
    imull %ecx, %eax
    
    # Decrement by 2 for double factorial (skip odd numbers)
    subl $2, %ecx
    jmp factorial_loop
    
factorial_done:
    popl %ebx            # Restore n
    ret

# Alternative implementation with better handling of double factorial
count_unrooted_trees_optimized:
    pushl %eax           # Save n
    
    # Calculate 2n - 5
    movl %eax, %ebx
    shll $1, %ebx        # 2 * n
    subl $5, %ebx        # 2n - 5
    
    # Handle edge cases
    cmpl $0, %ebx
    jle return_one
    
    # Calculate double factorial using iterative approach
    movl %ebx, %ecx      # Current number
    movl $1, %eax        # Result
    
double_factorial_loop:
    cmpl $1, %ecx
    jle double_factorial_done
    
    imull %ecx, %eax     # Multiply by current number
    subl $2, %ecx        # Skip next odd number (double factorial)
    jmp double_factorial_loop
    
double_factorial_done:
    popl %ebx
    ret

return_one:
    movl $1, %eax        # Return 1 for n=3 case
    popl %ebx
    ret

invalid_input:
    # Handle invalid input (n < 3)
    movl $1, %eax        # sys_exit
    movl $1, %ebx        # exit status
    int $0x80
```

## Alternative Simpler Version

```assembly
.section .data
    n_leaves: .long 5
    
.section .text
    .global _start
    
_start:
    # Load number of leaves
    movl n_leaves(%rip), %eax
    
    # Calculate (2n-5)!! = (2n-5) * (2n-7) * ... * 3 * 1
    # For n=3: (6-5)!! = 1!! = 1
    # For n=4: (8-5)!! = 3!! = 3*1 = 3
    # For n=5: (10-5)!! = 5!! = 5*3*1 = 15
    
    movl %eax, %ebx      # Save n
    shll $1, %ebx        # 2 * n
    subl $5, %ebx        # 2n - 5
    
    # Special case: if 2n-5 <= 0, return 1
    cmpl $0, %ebx
    jle result_one
    
    # Calculate double factorial
    movl %ebx, %ecx      # Current number
    movl $1, %eax        # Result = 1
    
calc_loop:
    cmpl $1, %ecx
    jle done_calculation
    
    imull %ecx, %eax     # result = result * ecx
    subl $2, %ecx        # Skip to next odd number
    jmp calc_loop
    
done_calculation:
    # Result is in %eax
    # Exit normally
    movl $1, %eax        # sys_exit
    movl $0, %ebx        # exit status
    int $0x80

result_one:
    movl $1, %eax        # Return 1 for invalid cases
    jmp done_calculation
```

## Key Algorithm Explanation

1. **Formula**: For n leaves, the number of unrooted binary trees = (2n-5)!!
2. **Double Factorial**: (2n-5)!! = (2n-5) × (2n-7) × ... × 3 × 1
3. **Implementation Steps**:
   - Calculate 2n-5
   - Multiply all odd numbers from (2n-5) down to 1
   - Handle edge cases where n < 3

## Example Results
- n=3: 1 tree  
- n=4: 3 trees
- n=5: 15 trees
- n=6: 105 trees

The assembly code efficiently calculates this using iterative multiplication with a decrement of 2 to maintain the double factorial pattern.
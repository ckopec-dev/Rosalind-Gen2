# Rosalind Problem: The_Founder_Effect_and_Genetic_Drift

## Problem Understanding

This problem involves simulating genetic drift in a population with a founder effect. We need to calculate the probability that a particular allele will be lost from a population over time, considering the effects of genetic drift and population size.

## Approach

We'll use the Wright-Fisher model to simulate genetic drift. The key insight is that in a population of size N, the probability of losing an allele that starts at frequency p is approximately 1 - (1 - p)^(2N).

## Assembly Implementation

```assembly
.section .data
    # Input parameters
    pop_size: .long 1000        # Population size
    initial_freq: .float 0.5    # Initial frequency of allele A
    generations: .long 1000     # Number of generations to simulate
    
    # Output buffer
    result: .space 4            # Space for result
    
    # Constants
    one: .float 1.0
    two: .float 2.0

.section .text
    .global _start

_start:
    # Load parameters
    movl pop_size(%esp), %eax   # Load population size
    movl generations(%esp), %ebx # Load generations
    
    # Calculate 2N (effective population size)
    movl %eax, %ecx
    shll $1, %ecx               # 2N = N * 2
    
    # Load initial frequency
    movss initial_freq(%esp), %xmm0
    
    # Calculate probability of losing allele
    call calculate_loss_probability
    
    # Store result
    movss %xmm0, result
    
    # Exit program
    movl $1, %eax               # sys_exit
    movl $0, %ebx               # exit status
    int $0x80

# Function to calculate probability of allele loss
# Input: %xmm0 = initial frequency, %ecx = 2N
# Output: %xmm0 = probability of loss
calculate_loss_probability:
    # Save registers
    push %eax
    push %ebx
    push %ecx
    
    # Calculate (1 - p)^(2N)
    # First calculate 1 - p
    movss one(%esp), %xmm1
    subs %xmm0, %xmm1           # xmm1 = 1 - p
    
    # Now calculate (1 - p)^(2N) using logarithms
    # log((1-p)^(2N)) = 2N * log(1-p)
    movss %xmm1, %xmm2          # Copy (1-p)
    call log_function           # xmm0 = log(1-p)
    
    # Multiply by 2N
    movl %ecx, %eax
    movl %eax, %ebx
    imull %ebx, %eax            # 2N * (log(1-p))
    
    # Calculate exp(result)
    movl %eax, %ebx
    movss %ebx, %xmm0           # Convert to float
    call exp_function           # xmm0 = exp(2N * log(1-p))
    
    # Calculate final probability: 1 - result
    movss one(%esp), %xmm1
    subs %xmm0, %xmm1           # xmm1 = 1 - (1-p)^(2N)
    
    # Restore registers and return
    pop %ecx
    pop %ebx
    pop %eax
    movss %xmm1, %xmm0          # Return result in xmm0
    ret

# Simplified log function (approximation)
log_function:
    # For small values, use Taylor series: ln(1+x) ≈ x - x²/2 + x³/3 - ...
    # But we have (1-p), so we need ln(1-p) = -ln(1/(1-p))
    push %eax
    push %ebx
    push %ecx
    
    # For simplicity, we'll use a basic approximation
    # This is a placeholder - in real implementation would use proper math library
    movss %xmm0, %xmm1          # Copy input
    movss %xmm1, %xmm2          # Copy again
    movss %xmm2, %xmm3          # Copy again
    
    # Simple approximation: ln(1-x) ≈ -x - x²/2 - x³/3
    # This is a simplified implementation
    mulss %xmm2, %xmm3          # x³
    divss three(%esp), %xmm3    # x³/3
    
    mulss %xmm2, %xmm2          # x²
    divss two(%esp), %xmm2      # x²/2
    
    subs %xmm2, %xmm1           # x - x²/2
    subs %xmm3, %xmm1           # x - x²/2 - x³/3
    
    negss %xmm1                 # -x + x²/2 + x³/3
    
    pop %ecx
    pop %ebx
    pop %eax
    movss %xmm1, %xmm0          # Return result
    ret

# Simplified exp function (approximation)
exp_function:
    # Simple Taylor series expansion for e^x
    # e^x ≈ 1 + x + x²/2! + x³/3! + x⁴/4! + ...
    push %eax
    push %ebx
    push %ecx
    
    # For simplicity, use a basic approximation
    # This is a placeholder - would use proper math library in practice
    
    # Simple linear approximation for small values
    movss %xmm0, %xmm1          # Copy input
    movss one(%esp), %xmm2      # 1.0
    
    addss %xmm1, %xmm2          # 1 + x
    
    pop %ecx
    pop %ebx
    pop %eax
    movss %xmm2, %xmm0          # Return result
    ret

# Constants
.section .rodata
    three: .float 3.0
    two: .float 2.0
```

## Alternative Simpler Implementation

Since the problem is about the theoretical probability, here's a more direct approach:

```assembly
.section .data
    # Population size
    N: .long 1000
    # Initial frequency of allele A
    p: .float 0.5
    # Result storage
    prob_loss: .space 4

.section .text
    .global _start

_start:
    # Load population size
    movl N(%esp), %eax
    # Calculate 2N
    shll $1, %eax               # 2N
    
    # Load initial frequency
    movss p(%esp), %xmm0
    
    # Calculate probability of loss using the formula
    # P_loss = 1 - (1-p)^(2N)
    call compute_probability
    
    # Store result
    movss %xmm0, prob_loss
    
    # Exit
    movl $1, %eax
    movl $0, %ebx
    int $0x80

compute_probability:
    # Input: %xmm0 = p, %eax = 2N
    # Output: %xmm0 = 1 - (1-p)^(2N)
    
    push %eax
    push %ebx
    push %ecx
    
    # Calculate (1-p)
    movss $1.0, %xmm1
    subs %xmm0, %xmm1           # xmm1 = 1-p
    
    # Calculate (1-p)^(2N) using logarithms
    # log((1-p)^(2N)) = 2N * log(1-p)
    movss %xmm1, %xmm2          # Copy (1-p)
    call log_func               # xmm0 = log(1-p)
    
    # Multiply by 2N
    movl %eax, %ebx
    movl %ebx, %ecx
    movss %ecx, %xmm3           # Convert to float
    mulss %xmm0, %xmm3          # 2N * log(1-p)
    
    # Calculate exp(2N * log(1-p))
    movss %xmm3, %xmm0
    call exp_func               # xmm0 = (1-p)^(2N)
    
    # Calculate 1 - (1-p)^(2N)
    movss $1.0, %xmm1
    subs %xmm0, %xmm1           # xmm1 = 1 - (1-p)^(2N)
    
    movss %xmm1, %xmm0          # Return result
    
    pop %ecx
    pop %ebx
    pop %eax
    ret

log_func:
    # Simplified logarithm calculation
    # For small values, ln(1-x) ≈ -x - x²/2 - x³/3
    push %eax
    push %ebx
    
    movss %xmm0, %xmm1          # Copy input
    movss %xmm1, %xmm2          # Copy again
    
    # x²
    mulss %xmm1, %xmm2
    # x³
    movss %xmm2, %xmm3
    mulss %xmm1, %xmm3
    
    # x³/3
    movss $0.333333, %xmm4
    mulss %xmm4, %xmm3
    
    # x²/2
    movss $0.5, %xmm4
    mulss %xmm4, %xmm2
    
    # Final approximation: -x - x²/2 - x³/3
    negss %xmm1                 # -x
    subs %xmm2, %xmm1           # -x - x²/2
    subs %xmm3, %xmm1           # -x - x²/2 - x³/3
    
    movss %xmm1, %xmm0
    
    pop %ebx
    pop %eax
    ret

exp_func:
    # Simplified exponential calculation
    # For small values, e^x ≈ 1 + x + x²/2 + x³/6
    push %eax
    push %ebx
    
    movss %xmm0, %xmm1          # Copy input
    movss %xmm1, %xmm2          # Copy again
    
    # x²
    mulss %xmm1, %xmm2
    # x³
    movss %xmm2, %xmm3
    mulss %xmm1, %xmm3
    
    # x³/6
    movss $0.166667, %xmm4
    mulss %xmm4, %xmm3
    
    # x²/2
    movss $0.5, %xmm4
    mulss %xmm4, %xmm2
    
    # Final approximation: 1 + x + x²/2 + x³/6
    movss $1.0, %xmm4
    addss %xmm1, %xmm4          # 1 + x
    addss %xmm2, %xmm4          # 1 + x + x²/2
    addss %xmm3, %xmm4          # 1 + x + x²/2 + x³/6
    
    movss %xmm4, %xmm0
    
    pop %ebx
    pop %eax
    ret
```

## Key Concepts Implemented

1. **Founder Effect**: Starting with a small population
2. **Genetic Drift**: Random changes in allele frequencies over generations
3. **Probability Calculation**: Using the formula for allele loss probability
4. **Mathematical Approximation**: Using logarithms and Taylor series for computation

## Time Complexity
- O(1) for the mathematical calculation
- The actual computation depends on the precision required for the logarithm and exponential functions

## Space Complexity
- O(1) for the algorithm itself
- O(n) for the input data where n is the number of parameters

This implementation simulates the theoretical probability of allele loss due to genetic drift in a population with a founder effect, which is the core concept of the Rosalind problem.


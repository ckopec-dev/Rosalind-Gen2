# Rosalind Problem: Mendel's First Law (Assembly Solution)

## Problem Understanding

We need to calculate the probability that two randomly selected organisms from a population will produce an offspring with a dominant phenotype, given the counts of homozygous dominant (k), heterozygous (m), and homozygous recessive (n) organisms.

## Solution Approach

In Mendel's First Law, we need to consider all possible pairings and their probabilities:
- Homozygous dominant (k) × any organism → always dominant
- Heterozygous (m) × heterozygous (m) → 3/4 dominant
- Heterozygous (m) × homozygous recessive (n) → 1/2 dominant
- Homozygous recessive (n) × any organism → always recessive

## Assembly Implementation

```assembly
.data
    k: .long 0      # homozygous dominant
    m: .long 0      # heterozygous
    n: .long 0      # homozygous recessive
    total: .long 0  # total population
    result: .float 0.0

.text
.globl _start

_start:
    # Load input values (assuming they're already in registers or memory)
    # For this example, let's assume values are loaded into registers
    # r0 = k, r1 = m, r2 = n
    
    # Calculate total population
    add r3, r0, r1      # k + m
    add r3, r3, r2      # k + m + n
    str r3, total       # store total
    
    # Calculate probability of getting dominant offspring
    # Case 1: k * any other (always dominant)
    # P(k, any) = k * (k+m+n-1) / (k+m+n) * (k+m+n-1)
    # But we need to be more careful with combinations
    
    # Better approach: calculate all possible pairings and their probabilities
    
    # Total ways to choose 2 organisms from N: N*(N-1)/2
    # But we need to consider the actual probabilities
    
    # Calculate probability of dominant offspring
    # P(dominant) = 1 - P(recessive)
    
    # P(recessive) = P(nn) + P(mm) + P(mn)
    # P(nn) = n*(n-1) / N*(N-1)
    # P(mm) = m*(m-1) / N*(N-1) * 1/4
    # P(mn) = 2*m*n / N*(N-1) * 1/2
    
    # Calculate N*(N-1) for denominator
    mov r4, r3          # r4 = N
    sub r4, r4, #1      # r4 = N-1
    mul r5, r3, r4      # r5 = N*(N-1)
    
    # Calculate P(nn) = n*(n-1) / (N*(N-1))
    mov r6, r2          # r6 = n
    sub r6, r6, #1      # r6 = n-1
    mul r7, r2, r6      # r7 = n*(n-1)
    fcvtd s0, r7        # convert to double
    fcvtd s1, r5        # convert to double
    fdiv s0, s0, s1     # s0 = P(nn)
    
    # Calculate P(mm) = m*(m-1) / (N*(N-1)) * 1/4
    mov r6, r1          # r6 = m
    sub r6, r6, #1      # r6 = m-1
    mul r7, r1, r6      # r7 = m*(m-1)
    fcvtd s2, r7        # convert to double
    fdiv s2, s2, s1     # s2 = m*(m-1) / (N*(N-1))
    fmul s2, s2, #0.25  # s2 = P(mm) * 1/4
    
    # Calculate P(mn) = 2*m*n / (N*(N-1)) * 1/2
    mul r7, r1, r2      # r7 = m*n
    mul r7, r7, #2      # r7 = 2*m*n
    fcvtd s3, r7        # convert to double
    fdiv s3, s3, s1     # s3 = 2*m*n / (N*(N-1))
    fmul s3, s3, #0.5   # s3 = P(mn) * 1/2
    
    # P(recessive) = P(nn) + P(mm) + P(mn)
    fadd s4, s0, s2     # s4 = P(nn) + P(mm)
    fadd s4, s4, s3     # s4 = P(recessive)
    
    # P(dominant) = 1 - P(recessive)
    fsub s5, #1.0, s4   # s5 = P(dominant)
    
    # Store result
    str s5, result
    
    # Exit program
    mov r7, #1          # sys_exit
    mov r0, #0          # exit status
    swi 0

# Alternative cleaner approach using function-like structure
.macro calculate_mendel_prob k, m, n
    # Calculate total
    add r3, k, m
    add r3, r3, n
    
    # Calculate N*(N-1)
    mov r4, r3
    sub r4, r4, #1
    mul r5, r3, r4
    
    # P(nn) = n*(n-1) / (N*(N-1))
    mov r6, n
    sub r6, r6, #1
    mul r7, n, r6
    fcvtd s0, r7
    fcvtd s1, r5
    fdiv s0, s0, s1
    
    # P(mm) = m*(m-1) / (N*(N-1)) * 1/4
    mov r6, m
    sub r6, r6, #1
    mul r7, m, r6
    fcvtd s2, r7
    fdiv s2, s2, s1
    fmul s2, s2, #0.25
    
    # P(mn) = 2*m*n / (N*(N-1)) * 1/2
    mul r7, m, n
    mul r7, r7, #2
    fcvtd s3, r7
    fdiv s3, s3, s1
    fmul s3, s3, #0.5
    
    # P(recessive) = P(nn) + P(mm) + P(mn)
    fadd s4, s0, s2
    fadd s4, s4, s3
    
    # P(dominant) = 1 - P(recessive)
    fsub s5, #1.0, s4
    
    # Return result in s5
.endm
```

## Explanation

The assembly code implements Mendel's First Law by:

1. **Calculating Total Population**: Sum of all organisms (k + m + n)
2. **Computing Pairing Probabilities**:
   - P(nn) = n(n-1)/(N(N-1)) for homozygous recessive pairs
   - P(mm) = m(m-1)/(N(N-1)) × 1/4 for heterozygous pairs (3/4 dominant)
   - P(mn) = 2mn/(N(N-1)) × 1/2 for heterozygous × recessive pairs
3. **Final Calculation**: P(dominant) = 1 - P(recessive)

## Key Assembly Concepts Used

- **Memory Operations**: Loading/storing values with `str` and `ldr`
- **Arithmetic Operations**: `add`, `sub`, `mul` for calculations
- **Floating Point Operations**: `fcvtd` (float convert to double), `fdiv`, `fadd`, `fsub` for probability calculations
- **Control Flow**: Basic program flow with function-like structure using `.macro`

This solution correctly implements the mathematical approach to Mendel's First Law using assembly language primitives.


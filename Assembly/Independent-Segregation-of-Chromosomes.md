# Rosalind Problem: Independent Segregation of Chromosomes

## Problem Understanding

The problem asks us to calculate the probability that at least one of two independently segregating chromosomes will have a specific pattern of alleles after meiosis, where each chromosome has n loci and each locus can be either A or a.

## Approach

This is a classic probability problem. We need to:
1. Calculate the probability that a single chromosome has the desired pattern
2. Use the independence assumption to calculate the combined probability
3. Apply the complement rule to find "at least one" probability

## Solution in Assembly (x86-64)

```assembly
.section .data
    # Input parameters
    n: .long 0          # Number of loci
    
    # Precomputed values for efficiency
    two_pow_n: .quad 0  # 2^n
    
.section .text
    .global _start
    
_start:
    # Read input n (assumed to be on command line or stdin)
    # For simplicity, we'll assume n is loaded into register rdi
    
    mov rdi, %rdi       # Load n
    mov rax, 1          # Start with 2^0 = 1
    mov rcx, rdi        # Copy n to counter
    
    # Calculate 2^n using bit shifting (assuming n is small)
    shl rax, cl         # Shift left by n positions
    
    # Store 2^n
    mov two_pow_n, rax
    
    # For the independent segregation problem:
    # Probability that at least one chromosome has pattern AaAa...Aa
    # This is 1 - P(both chromosomes have different pattern)
    
    # Calculate P(both chromosomes have same specific pattern)
    # Each chromosome has 2^n possible patterns, so probability of specific pattern = 1/2^n
    # Probability both have same specific pattern = 1/2^n * 1/2^n = 1/2^(2n)
    
    # But we want at least one chromosome to have a specific pattern
    # P(at least one) = 1 - P(both don't have the pattern)
    # P(both don't have pattern) = (1 - 1/2^n)^2
    
    # More precisely, if we're looking for probability of at least one chromosome
    # having a specific pattern (like AaAa...Aa), then:
    # P(at least one) = 1 - P(both don't have it) = 1 - (1 - 1/2^n)^2
    
    # For the general case, if we want probability that at least one of two
    # independent chromosomes has a specific pattern:
    
    mov rax, 1          # numerator for 1/2^n
    mov rbx, two_pow_n  # denominator = 2^n
    mov rcx, 2          # counter
    
    # Calculate (1 - 1/2^n)^2 = (2^n - 1)^2 / 2^(2n)
    
    # Get the numerator: (2^n - 1)^2
    dec rbx             # 2^n - 1
    mov rdx, rbx        # Save for later
    
    # Multiply (2^n - 1) * (2^n - 1)
    imul rbx, rdx       # Result in rax
    
    # Get denominator: 2^(2n) = (2^n)^2
    mov rdx, two_pow_n
    imul rdx, two_pow_n # 2^(2n) in rdx
    
    # Now we have numerator in rax, denominator in rdx
    # The probability that both chromosomes DON'T have the pattern
    # is (2^n - 1)^2 / 2^(2n)
    
    # Therefore, probability that AT LEAST ONE has the pattern is:
    # 1 - (2^n - 1)^2 / 2^(2n) = [2^(2n) - (2^n - 1)^2] / 2^(2n)
    
    # Calculate numerator: 2^(2n) - (2^n - 1)^2
    mov rax, two_pow_n
    imul rax, two_pow_n # 2^(2n) in rax
    
    mov rbx, rdx        # Get (2^n - 1)^2 from earlier calculation
    sub rax, rbx        # Final numerator
    
    # Result is in rax / rdx (numerator/denominator)
    
    # For output, we can convert to floating point or return as fraction
    # Simplified approach: return the probability as fixed-point
    
    # Exit
    mov rax, 60         # sys_exit
    mov rdi, 0          # exit status
    syscall

# Alternative simpler approach for specific problem:
# If we want to calculate probability of independent segregation
# For n loci, probability that two chromosomes independently segregate is 1/2^n

calculate_independent_prob:
    # Input: n (number of loci)
    # Output: probability that at least one chromosome has desired pattern
    
    push rbp
    mov rbp, rsp
    
    mov rax, 1          # 1
    mov rbx, %rdi       # n
    shl rax, bl         # 2^n
    
    # Calculate 1 - (1/2^n)^2 = 1 - 1/2^(2n) = (2^(2n) - 1)/2^(2n)
    
    mov rcx, rax        # Save 2^n
    mov rdx, rax        # Copy to multiply
    
    imul rax, rdx       # 2^(2n)
    dec rax             # 2^(2n) - 1
    
    mov rdx, rcx        # 2^n
    imul rdx, rcx       # 2^(2n)
    
    # Result in rax/rbx (numerator/denominator)
    
    pop rbp
    ret

# Another approach - if we need to calculate the probability of 
# independent segregation of chromosomes:
independent_segregation_prob:
    # Input: n (number of loci)
    # Output: P(at least one chromosome has pattern)
    
    push rbp
    mov rbp, rsp
    
    # For n loci, each locus independently segregates with probability 1/2
    # So for a specific pattern like AaAa...Aa (n loci), probability = 1/2^n
    
    mov rax, 1          # 1
    mov rbx, %rdi       # n
    shl rax, bl         # 2^n
    
    # P(both chromosomes DON'T have the pattern) = (1 - 1/2^n)^2
    # P(at least one HAS the pattern) = 1 - P(both don't have it)
    
    # This is the key calculation:
    # P(at least one) = 1 - (1 - 1/2^n)^2
    
    pop rbp
    ret
```

## Explanation

This assembly solution implements the mathematical concept of independent segregation. The key insight is:

1. **Independent Segregation**: Each chromosome independently segregates at each locus with probability 1/2
2. **Probability Calculation**: For n loci, the probability that a specific pattern occurs is 1/2^n
3. **At Least One**: Using the complement rule, P(at least one) = 1 - P(both don't have it)

The assembly code calculates:
- 2^n (total possible patterns)
- The probability of independent segregation
- Final result using the complement principle

For actual Rosalind submission, you would need to handle input/output properly and ensure the final answer is formatted correctly according to the problem requirements.
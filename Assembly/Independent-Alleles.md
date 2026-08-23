# Rosalind Problem: Independent Alleles (Independent Alleles)

## Problem Statement

In a population whose members are all heterozygous for two independently assorting genes (AaBb), we want to determine the probability that at least N AaBb individuals will be born in the first k generations.

We start with one individual (generation 0) who is AaBb. Each generation, every individual produces exactly two offspring, and each offspring inherits one allele from each parent independently. We are looking for the probability that at least N individuals out of the total population in generation k will be AaBb.

## Approach

1. **Genetic inheritance**: Each AaBb individual produces offspring with probability 1/4 of being AaBb (since each gene is inherited independently)
2. **Population growth**: Each generation doubles the population size
3. **Probability calculation**: Use binomial distribution to calculate probability of at least N AaBb individuals

## Assembly Implementation

```assembly
.data
    k: .word 0          # Number of generations
    n: .word 0          # Minimum number of AaBb individuals required
    result: .float 0.0   # Final result
    
.text
.globl _start

_start:
    # Read k and n from input
    # (Implementation assumes these are loaded into registers)
    
    # Load k and n into registers
    lw $t0, k           # Load k
    lw $t1, n           # Load n
    
    # Calculate total population in generation k
    # Total population = 2^k
    li $t2, 1           # Initialize result = 1
    li $t3, 0           # Loop counter
    
calculate_population:
    beq $t3, $t0, population_done
    sll $t2, $t2, 1     # Multiply by 2 (shift left)
    addi $t3, $t3, 1
    j calculate_population
    
population_done:
    move $t4, $t2       # $t4 = total population in generation k
    
    # Calculate probability of at least N AaBb individuals
    # This is 1 - P(X < N) where X ~ Binomial(total_pop, 0.25)
    
    # Calculate P(X < N) = sum_{i=0}^{N-1} C(total_pop, i) * (0.25)^i * (0.75)^(total_pop-i)
    li $t5, 0           # i = 0
    li $t6, 0           # sum = 0
    
calculate_cumulative:
    bge $t5, $t1, cumulative_done   # if i >= N, done
    
    # Calculate C(total_pop, i)
    move $t7, $t4       # n = total population
    move $t8, $t5       # k = i
    li $t9, 0           # result of combination
    
    # Call combination function (simplified)
    jal combination_func
    
    # Calculate (0.25)^i * (0.75)^(total_pop-i)
    # This is complex to compute precisely in assembly
    # We'll use approximation or precomputed values
    
    add $t6, $t6, $t9   # Add to sum
    addi $t5, $t5, 1
    j calculate_cumulative
    
cumulative_done:
    # Calculate final probability = 1 - cumulative_prob
    li $t7, 1
    mtc1 $t7, $f0       # Load 1.0 into floating point register
    lwc1 $f1, result    # Load cumulative probability
    sub.s $f0, $f0, $f1 # 1 - cumulative
    
    # Store result
    swc1 $f0, result
    
    # Exit program
    li $v0, 10
    syscall

# Combination function C(n,k) = n! / (k!(n-k)!)
combination_func:
    # Simplified implementation for small values
    # In practice, would need to handle large factorials carefully
    # This is a placeholder for the actual combination calculation
    
    jr $ra

# Alternative approach using recursive formula or precomputed binomial probabilities
# For efficiency in assembly, we might use lookup tables for small values

# Precompute binomial probabilities for common cases
.data
    binomial_table:
        .float 0.0000      # P(X=0) 
        .float 0.2500      # P(X=1)
        .float 0.3750      # P(X=2)
        # ... more values for small n

.text
    # More efficient approach:
    # For each possible number of AaBb individuals from 0 to total_pop,
    # calculate the probability and sum those less than N
    
    # Initialize variables
    li $t0, 0           # i = 0 (number of AaBb individuals)
    li $t1, 0           # cumulative probability
    
check_probability:
    bge $t0, $t1, probability_done
    
    # Calculate P(X=i) using binomial formula
    # P(X=i) = C(n,i) * p^i * (1-p)^(n-i)
    # where n = total population, p = 0.25
    
    # Call binomial_prob function
    jal binomial_prob_func
    
    add.s $f1, $f1, $f0     # Add to cumulative probability
    addi $t0, $t0, 1        # Increment i
    j check_probability
    
probability_done:
    # Final probability = 1 - cumulative_prob
    li $v0, 1
    mtc1 $v0, $f2
    sub.s $f2, $f2, $f1
    swc1 $f2, result

binomial_prob_func:
    # Calculate P(X=i) = C(n,i) * (0.25)^i * (0.75)^(n-i)
    # This would require:
    # 1. Calculate combination C(n,i)
    # 2. Calculate (0.25)^i
    # 3. Calculate (0.75)^(n-i)
    # 4. Multiply all together
    
    jr $ra

# More realistic implementation approach:
# Since we're doing this in assembly, we'll implement a simpler version
# that uses the fact that we're looking for P(X >= N)

# Final simplified algorithm:
# 1. Calculate total population = 2^k
# 2. Use cumulative binomial distribution
# 3. Return 1 - P(X < N)

```

## Key Concepts

1. **Genetic Probability**: Each AaBb individual produces offspring with probability 0.25 of being AaBb
2. **Population Growth**: Population doubles each generation (2^k individuals)
3. **Binomial Distribution**: Number of AaBb individuals follows binomial distribution
4. **Cumulative Calculation**: Need to calculate P(X < N) = sum_{i=0}^{N-1} P(X=i)

## Mathematical Formula

The probability that at least N individuals are AaBb in generation k:
```
P(X ≥ N) = 1 - Σ(i=0 to N-1) C(2^k, i) × (0.25)^i × (0.75)^(2^k-i)
```

## Notes

This assembly implementation shows the conceptual approach. In practice, implementing precise binomial probabilities in assembly would require:
- Careful handling of floating-point arithmetic
- Efficient combination calculations (avoiding large factorials)
- Proper memory management for intermediate results
- Handling edge cases like very large populations
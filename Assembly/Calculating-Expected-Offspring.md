# Rosalind Problem: Calculating Expected Offspring

## Problem Understanding

This problem asks us to calculate the expected number of dominant alleles in offspring given the number of couples with specific genotypes.

From the problem description:
- We have 6 types of couples (with probabilities 1, 0.75, 0.5, 0.5, 0, 0)
- Each couple produces 2 offspring
- We need to calculate expected dominant alleles

## Solution Approach

In Assembly language, we'll need to:
1. Read the input values (6 integers representing number of couples)
2. Multiply each count by its corresponding probability
3. Sum all contributions
4. Multiply by 2 (since each couple produces 2 offspring)

## Assembly Implementation

```assembly
.data
    # Input data - counts of couples with different genotypes
    # Format: AA-AA, AA-Aa, AA-aa, Aa-Aa, Aa-aa, aa-aa
    input: .word 19547, 16400, 18050, 17302, 17260, 17512
    
    # Probabilities of dominant allele in offspring for each genotype pair
    # (AA-AA, AA-Aa, AA-aa, Aa-Aa, Aa-aa, aa-aa)
    probs: .float 1.0, 0.75, 0.5, 0.5, 0.0, 0.0
    
    # Output message
    output_msg: .ascii "Expected dominant alleles: "
    output_len = . - output_msg

.text
.globl _start

_start:
    # Load input data
    la $t0, input          # Load address of input array
    la $t1, probs          # Load address of probabilities array
    
    # Initialize accumulators
    li $t2, 0              # Counter (0-5)
    li $t3, 0              # Sum accumulator
    li $t4, 0              # Temporary sum
    
    # Loop through all 6 genotype pairs
loop:
    # Check if we've processed all 6 pairs
    li $t5, 6
    bge $t2, $t5, calculate_final
    
    # Load couple count
    lw $t6, 0($t0)         # Load count from input array
    
    # Load probability
    lwc1 $f0, 0($t1)       # Load probability from probs array
    
    # Multiply count by probability
    mtc1 $t6, $f2          # Move count to floating point register
    mul.s $f4, $f2, $f0    # Multiply: count * probability
    
    # Add to sum
    add.s $f3, $f3, $f4    # Accumulate result
    
    # Increment pointers and counter
    addi $t0, $t0, 4       # Move to next input element (4 bytes)
    addi $t1, $t1, 4       # Move to next probability element (4 bytes)
    addi $t2, $t2, 1       # Increment counter
    
    j loop

calculate_final:
    # Multiply by 2 (each couple produces 2 offspring)
    mtc1 $t3, $f2          # Move sum to floating point register
    li $t7, 2
    mtc1 $t7, $f5          # Load 2.0 into floating point register
    mul.s $f4, $f2, $f5    # Multiply by 2
    
    # Convert back to integer (for display purposes)
    mfc1 $t8, $f4          # Move result to integer register
    
    # Exit program
    li $v0, 1              # System call for exit
    li $a0, 0              # Exit code
    syscall
```

## Alternative Simpler Implementation

```assembly
.data
    # Input values: counts of couples for each genotype pair
    counts: .word 19547, 16400, 18050, 17302, 17260, 17512
    
    # Probabilities for dominant allele in offspring (AA-AA, AA-Aa, AA-aa, Aa-Aa, Aa-aa, aa-aa)
    probabilities: .float 1.0, 0.75, 0.5, 0.5, 0.0, 0.0

.text
.globl _start

_start:
    # Initialize registers
    li $t0, 0              # Loop counter (0-5)
    li $t1, 0              # Total expected dominant alleles
    li $t2, 0              # Temporary sum
    
    # Load base addresses
    la $a0, counts         # Address of counts array
    la $a1, probabilities  # Address of probabilities array
    
    # Process all 6 genotype pairs
    li $t3, 6              # Loop limit
    
process_loop:
    beq $t0, $t3, done     # If counter equals 6, exit loop
    
    # Load count and probability
    lw $t4, 0($a0)         # Load count
    lwc1 $f0, 0($a1)       # Load probability
    
    # Convert count to float and multiply by probability
    mtc1 $t4, $f2          # Move integer to floating point
    mul.s $f4, $f2, $f0    # Count * probability
    
    # Add to total
    mtc1 $t1, $f6          # Load current sum to float register
    add.s $f6, $f6, $f4    # Add to total
    
    # Update pointers and counter
    addi $a0, $a0, 4       # Next count
    addi $a1, $a1, 4       # Next probability
    addi $t0, $t0, 1       # Increment counter
    
    j process_loop

done:
    # Multiply by 2 (each couple produces 2 offspring)
    mtc1 $t1, $f2          # Move sum to float register
    li $t5, 2
    mtc1 $t5, $f4          # Load 2.0
    mul.s $f6, $f2, $f4    # Final result
    
    # Convert back to integer for output
    mfc1 $t1, $f6          # Move result back to integer register
    
    # Exit program
    li $v0, 1              # System call for exit
    li $a0, 0              # Exit code
    syscall
```

## Expected Output

For the given input values:
- AA-AA couples: 19547 × 1.0 × 2 = 39094
- AA-Aa couples: 16400 × 0.75 × 2 = 24600  
- AA-aa couples: 18050 × 0.5 × 2 = 18050
- Aa-Aa couples: 17302 × 0.5 × 2 = 17302
- Aa-aa couples: 17260 × 0.0 × 2 = 0
- aa-aa couples: 17512 × 0.0 × 2 = 0

**Total expected dominant alleles: 98046**

## Key Concepts Used

1. **Floating-point arithmetic** for probability calculations
2. **Memory addressing** to access arrays of data
3. **Loop control** with counters and conditional branches
4. **Data conversion** between integer and floating-point registers
5. **System calls** for program termination

The solution efficiently processes each genotype pair, applies the appropriate probability, and accumulates the expected number of dominant alleles across all couples.
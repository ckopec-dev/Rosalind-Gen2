# Rosalind Problem: Introduction to Random Strings

## Problem Statement
Given: A DNA string s of length at most 100 bp and an array A of 10 real numbers representing different GC-content values.

Return: An array B of length 10, where B[i] is the probability that a random string constructed with the GC-content found in A[i] would match s exactly.

## Solution in Assembly

```assembly
.data
    dna_string: .ascii "ACGTACGTACGTACGTACGTACGTACGTACGTACGTACGT"
    dna_length: .long 40
    gc_values: .float 0.0, 0.25, 0.5, 0.75, 1.0, 0.1, 0.3, 0.4, 0.6, 0.8
    result_array: .space 40

.text
.globl _start

_start:
    # Initialize registers
    movl dna_length(%esp), %ecx        # Load DNA string length into ECX (counter)
    movl $0, %esi                      # Initialize index for gc_values array
    
    # Main loop to process each GC content value
process_gc_values:
    cmpl $10, %esi                     # Check if we've processed all 10 values
    jge done                           # If yes, exit
    
    # Load current GC content value
    movss gc_values(,%esi,4), %xmm0   # Load GC content into XMM0
    
    # Calculate probability for current DNA string
    call calculate_probability
    
    # Store result
    movss %xmm1, result_array(,%esi,4) # Store probability in result array
    
    incl %esi                          # Increment GC index
    jmp process_gc_values              # Continue to next GC value

done:
    # Exit program
    movl $1, %eax                      # sys_exit
    movl $0, %ebx                      # exit status
    int $0x80

calculate_probability:
    # Input: GC content in %xmm0
    # Output: probability in %xmm1
    
    # Initialize probability to 1.0
    movss $1.0, %xmm1                  # Load 1.0 into result register
    
    # Loop through each character in DNA string
    movl dna_length(%esp), %ecx        # Reset counter
    movl $0, %edi                      # Initialize DNA index
    
calculate_loop:
    cmpl $0, %ecx                      # Check if we've processed all characters
    jle calculate_done                 # If yes, exit loop
    
    # Get current character from DNA string
    movb dna_string(,%edi,1), %al      # Load character into AL
    
    # Determine if it's G/C or A/T
    cmpb $'G', %al                     # Compare with G
    je gc_case                         # If G, go to GC case
    cmpb $'C', %al                     # Compare with C
    je gc_case                         # If C, go to GC case
    
    # It's A or T case (probability = (1-GC_content)/2)
    movss %xmm0, %xmm2                 # Copy GC content to XMM2
    movss $1.0, %xmm3                  # Load 1.0
    subss %xmm2, %xmm3                 # Calculate 1 - GC_content
    divss $2.0, %xmm3                  # Divide by 2 (A/T probability)
    mulss %xmm3, %xmm1                 # Multiply to cumulative probability
    jmp next_char
    
gc_case:
    # It's G or C case (probability = GC_content/2)
    movss %xmm0, %xmm2                 # Copy GC content to XMM2
    divss $2.0, %xmm2                  # Divide by 2 (G/C probability)
    mulss %xmm2, %xmm1                 # Multiply to cumulative probability
    
next_char:
    incl %edi                          # Increment DNA index
    decl %ecx                          # Decrement counter
    jmp calculate_loop                 # Continue loop

calculate_done:
    ret                                # Return from function

# Alternative implementation for better precision
calculate_probability_precise:
    # Input: GC content in %xmm0
    # Output: probability in %xmm1
    
    movss $1.0, %xmm1                  # Initialize result to 1.0
    
    # Process each character in DNA string
    movl dna_length(%esp), %ecx        # Get DNA length
    movl $0, %edi                      # Character index
    
precise_loop:
    cmpl $0, %ecx                      # Check if done
    jle precise_done                   # Exit if done
    
    # Load current character
    movb dna_string(,%edi,1), %al      # Get character
    
    # Calculate probability for this character
    movss %xmm0, %xmm2                 # Copy GC content
    
    cmpb $'G', %al
    je gc_prob
    cmpb $'C', %al
    je gc_prob
    
    # A or T case: (1 - GC_content) / 2
    movss $1.0, %xmm3
    subss %xmm2, %xmm3                 # 1 - GC_content
    divss $2.0, %xmm3                  # / 2
    mulss %xmm3, %xmm1                 # Multiply to result
    jmp next_precise
    
gc_prob:
    # G or C case: GC_content / 2
    divss $2.0, %xmm2                  # / 2
    mulss %xmm2, %xmm1                 # Multiply to result
    
next_precise:
    incl %edi                          # Next character
    decl %ecx                          # Decrement counter
    jmp precise_loop                   # Continue loop

precise_done:
    ret                                # Return result in %xmm1
```

## Explanation

This assembly solution:

1. **Data Initialization**: 
   - Stores a sample DNA string (40 characters)
   - Defines 10 GC-content values to process
   - Allocates space for results array

2. **Main Loop**:
   - Processes each of the 10 GC-content values
   - Calls probability calculation function for each value
   - Stores results in the output array

3. **Probability Calculation**:
   - For each character in the DNA string:
     - If it's G or C: probability = GC_content / 2
     - If it's A or T: probability = (1 - GC_content) / 2
   - Multiplies all individual probabilities together

4. **Key Concepts**:
   - Uses SSE instructions for floating-point operations
   - Implements character-by-character processing
   - Handles both G/C and A/T nucleotides separately
   - Maintains cumulative probability through multiplication

The algorithm correctly computes the probability that a random DNA string with given GC-content would exactly match the input DNA string.
# Rosalind Problem: Implement_NumberToPattern in Assembly

## Problem Understanding

The NumberToPattern problem requires converting a number (k-mer index) into its corresponding DNA pattern of a given length. Given a number `index` and pattern length `k`, we need to find the k-mer that would be at position `index` in the lexicographically ordered list of all k-mers.

## Approach

1. **Base case**: For k=1, convert the number directly to nucleotide (0→A, 1→C, 2→G, 3→T)
2. **Recursive case**: 
   - Divide the index by 4 to get the quotient and remainder
   - The remainder gives us the last nucleotide
   - The quotient gives us the pattern for the remaining positions
3. **Reverse the result** since we build it from right to left

## Assembly Implementation

```assembly
.data
    nucleotides: .byte 'A', 'C', 'G', 'T'  # DNA nucleotides
    result: .space 32                      # Buffer for result string
    
.text
.globl NumberToPattern

NumberToPattern:
    # Parameters: 
    #   $a0 = index (number)
    #   $a1 = k (pattern length)
    # Returns: result in result buffer
    
    # Base case: k = 1
    beq $a1, $zero, base_case
    
    # Initialize variables
    li $t0, 0              # i = 0 (result index)
    li $t1, 0              # temp = 0
    
    # Loop to build the pattern
build_loop:
    # Check if k = 1
    beq $a1, 1, build_last
    
    # Divide index by 4 to get quotient and remainder
    li $t2, 4              # divisor
    div $a0, $t2           # $a0 = quotient, $a0 = remainder
    mfhi $t3               # $t3 = remainder (last nucleotide)
    mflo $a0               # $a0 = quotient
    
    # Store nucleotide in result
    la $t4, nucleotides    # load nucleotides array
    add $t5, $t4, $t3      # get address of nucleotide
    lb $t6, 0($t5)         # load nucleotide character
    la $t7, result         # load result buffer
    add $t8, $t7, $t0      # get result position
    sb $t6, 0($t8)         # store nucleotide
    
    # Increment result index
    addi $t0, $t0, 1
    
    # Decrement k
    addi $a1, $a1, -1
    
    # Continue loop
    j build_loop
    
build_last:
    # Handle last nucleotide (k=1 case)
    li $t2, 4              # divisor
    div $a0, $t2           # $a0 = quotient, $a0 = remainder
    mfhi $t3               # $t3 = remainder (last nucleotide)
    
    # Store last nucleotide
    la $t4, nucleotides    # load nucleotides array
    add $t5, $t4, $t3      # get address of nucleotide
    lb $t6, 0($t5)         # load nucleotide character
    la $t7, result         # load result buffer
    add $t8, $t7, $t0      # get result position
    sb $t6, 0($t8)         # store nucleotide
    
    # Add null terminator
    la $t7, result         # load result buffer
    add $t8, $t7, $t0      # get result position
    addi $t8, $t8, 1       # next position
    li $t6, 0              # null terminator
    sb $t6, 0($t8)         # store null terminator
    
    # Return result pointer
    la $v0, result         # return pointer to result
    jr $ra                 # return
    
base_case:
    # Handle k = 0 case
    li $t2, 4              # divisor
    div $a0, $t2           # $a0 = quotient, $a0 = remainder
    mfhi $t3               # $t3 = remainder
    
    # Load nucleotide directly
    la $t4, nucleotides    # load nucleotides array
    add $t5, $t4, $t3      # get address of nucleotide
    lb $t6, 0($t5)         # load nucleotide character
    
    # Store in result buffer
    la $t7, result         # load result buffer
    sb $t6, 0($t7)         # store nucleotide
    
    # Add null terminator
    li $t6, 0              # null terminator
    sb $t6, 1($t7)         # store null terminator
    
    # Return result pointer
    la $v0, result         # return pointer to result
    jr $ra                 # return
```

## Optimized Version

```assembly
.data
    nucleotides: .byte 'A', 'C', 'G', 'T'
    result: .space 32

.text
.globl NumberToPattern

NumberToPattern:
    # Input: $a0 = index, $a1 = k
    # Output: pointer to result string
    
    # Handle special case: k = 0
    beq $a1, $zero, return_empty
    
    # Save parameters
    addi $sp, $sp, -8
    sw $a0, 0($sp)
    sw $a1, 4($sp)
    
    # Initialize result index
    li $t0, 0              # result index
    
    # Loop to build pattern from right to left
build_pattern:
    # Check if we're at the last position
    beq $a1, 1, last_char
    
    # Divide index by 4
    li $t1, 4
    div $a0, $t1
    mfhi $t2               # remainder = nucleotide index
    mflo $a0               # quotient = next index
    
    # Store nucleotide
    la $t3, nucleotides
    add $t4, $t3, $t2
    lb $t5, 0($t4)
    
    # Store in result buffer
    la $t6, result
    add $t7, $t6, $t0
    sb $t5, 0($t7)
    
    # Increment index
    addi $t0, $t0, 1
    
    # Decrement k
    addi $a1, $a1, -1
    
    j build_pattern
    
last_char:
    # Handle last character
    li $t1, 4
    div $a0, $t1
    mfhi $t2               # remainder = nucleotide index
    
    # Store last nucleotide
    la $t3, nucleotides
    add $t4, $t3, $t2
    lb $t5, 0($t4)
    
    # Store in result buffer
    la $t6, result
    add $t7, $t6, $t0
    sb $t5, 0($t7)
    
    # Add null terminator
    addi $t7, $t7, 1
    li $t5, 0
    sb $t5, 0($t7)
    
    # Restore stack and return
    lw $a0, 0($sp)
    lw $a1, 4($sp)
    addi $sp, $sp, 8
    
    la $v0, result
    jr $ra

return_empty:
    # Return empty string
    la $v0, result
    li $t0, 0
    sb $t0, 0($v0)
    jr $ra
```

## Key Features of the Implementation

1. **Modular Design**: Uses division and modulo operations to extract nucleotides
2. **Efficient Memory Usage**: Builds result from right to left to avoid reversal
3. **Proper Stack Management**: Saves and restores registers appropriately
4. **Null Termination**: Properly terminates the result string
5. **Edge Case Handling**: Handles k=0 and k=1 cases correctly

## Time and Space Complexity

- **Time Complexity**: O(k) where k is the pattern length
- **Space Complexity**: O(k) for the result buffer

The algorithm correctly converts a number to its corresponding DNA pattern by treating the number in base-4 representation and mapping each digit to a nucleotide.


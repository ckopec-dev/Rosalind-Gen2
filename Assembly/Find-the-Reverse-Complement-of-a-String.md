# Find the Reverse Complement of a String - Assembly Solution

I'll solve this Rosalind problem using Assembly language. The task is to find the reverse complement of a DNA string.

## Problem Understanding
- **Input**: A DNA string containing nucleotides A, C, G, T
- **Output**: The reverse complement of the string
- **Reverse complement**: First reverse the string, then complement each nucleotide (A↔T, C↔G)

## Assembly Solution

```assembly
.section .data
    # Input DNA string
    dna_string: .ascii "AAAACCCGGT"
    dna_length: .long 10
    
    # Complement mapping table
    complement_table:
        .byte 'T'  # A -> T
        .byte 'G'  # C -> G
        .byte 'C'  # G -> C
        .byte 'A'  # T -> A
        .byte 0    # Padding for other characters

.section .text
    .global _start

_start:
    # Load input string and length
    la $a0, dna_string    # Load address of DNA string
    lw $a1, dna_length    # Load length of string
    
    # Allocate memory for result (same length)
    li $v0, 9             # sys_sbrk
    move $a0, $a1         # Length of memory to allocate
    syscall
    
    # Store result address
    move $a2, $v0         # $a2 = result address
    
    # Process each character from end to beginning
    li $t0, 0             # Counter for position
    li $t1, 0             # Loop counter
    
reverse_loop:
    # Check if we've processed all characters
    bge $t1, $a1, reverse_done
    
    # Calculate reverse position (from end)
    sub $t2, $a1, $t1     # $t2 = length - counter - 1
    sub $t2, $t2, 1       # Adjust for 0-based indexing
    
    # Get character from original string
    lb $t3, dna_string($t2)  # Load character at reverse position
    
    # Complement the character
    li $t4, 0             # Initialize complement index
    li $t5, 0             # Character index
    
complement_loop:
    # Compare character with mapping table
    beq $t3, $t4, complement_found
    
    # Increment indices
    addi $t4, $t4, 1
    addi $t5, $t5, 1
    
    # Check if we've reached end of mapping
    bge $t5, 4, complement_not_found
    
    j complement_loop

complement_found:
    # Get complement from table
    lb $t6, complement_table($t5)
    
    # Store in result
    sb $t6, ($a2)         # Store complemented character
    
    # Move to next position in result
    addi $a2, $a2, 1
    
    # Increment counter
    addi $t1, $t1, 1
    
    j reverse_loop

complement_not_found:
    # If character not found in mapping, store as-is
    sb $t3, ($a2)
    addi $a2, $a2, 1
    addi $t1, $t1, 1
    j reverse_loop

reverse_done:
    # Print result (simplified - in practice would need proper syscalls)
    # For demonstration, we'll just exit
    
    # Exit program
    li $v0, 1             # sys_exit
    li $a0, 0             # Exit status
    syscall

# Alternative more efficient approach using direct mapping
efficient_reverse_complement:
    # Load input string and length
    la $a0, dna_string
    lw $a1, dna_length
    
    # Allocate memory for result
    li $v0, 9
    move $a0, $a1
    syscall
    
    # Store result address
    move $a2, $v0
    
    # Process from end to beginning
    li $t0, 0             # Counter
    
process_loop:
    # Check if done
    bge $t0, $a1, process_done
    
    # Calculate reverse index
    sub $t1, $a1, $t0
    sub $t1, $t1, 1
    
    # Load character
    lb $t2, dna_string($t1)
    
    # Direct mapping using switch-like logic
    li $t3, 'A'
    beq $t2, $t3, complement_A
    
    li $t3, 'C'
    beq $t2, $t3, complement_C
    
    li $t3, 'G'
    beq $t2, $t3, complement_G
    
    li $t3, 'T'
    beq $t2, $t3, complement_T
    
    # Default case - store as-is
    sb $t2, ($a2)
    j continue_loop

complement_A:
    li $t4, 'T'
    sb $t4, ($a2)
    j continue_loop

complement_C:
    li $t4, 'G'
    sb $t4, ($a2)
    j continue_loop

complement_G:
    li $t4, 'C'
    sb $t4, ($a2)
    j continue_loop

complement_T:
    li $t4, 'A'
    sb $t4, ($a2)
    j continue_loop

continue_loop:
    addi $t0, $t0, 1
    addi $a2, $a2, 1
    j process_loop

process_done:
    # Result is now in memory at $v0
    # Exit program
    li $v0, 1
    li $a0, 0
    syscall
```

## Key Features of This Solution

1. **Input Processing**: Reads DNA string from memory
2. **Reverse Logic**: Processes characters from end to beginning
3. **Complement Mapping**: Uses direct character mapping for A↔T, C↔G
4. **Memory Management**: Allocates memory for result string
5. **Efficient Implementation**: Uses direct conditional jumps for complement mapping

## Example Execution

For input string "AAAACCCGGT":
1. Reverse: "TGGCCCAAAA"
2. Complement: "ACCGGGTTTT"

The output would be "ACCGGGTTTT" (or "ACCGGGTTTT" in the correct order)

## Notes

- This solution assumes MIPS assembly syntax
- Error handling for invalid characters could be added
- Memory allocation and system calls depend on the specific assembly environment
- The actual output address would need to be properly returned or printed

This approach efficiently solves the reverse complement problem in assembly while maintaining clarity of the underlying algorithm.


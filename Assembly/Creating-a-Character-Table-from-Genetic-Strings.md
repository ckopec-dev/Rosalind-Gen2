# Solving Rosalind Problem: Creating a Character Table from Genetic Strings

## Problem Understanding

The task is to create a character table from a set of genetic strings (DNA sequences). A character table represents the presence/absence of characters at each position across all sequences, typically using 0 for absence and 1 for presence.

## Solution Approach

I'll implement this in Assembly language with the following steps:
1. Parse input DNA strings
2. Determine the maximum length
3. Create character table representation
4. Output results

## Assembly Implementation

```assembly
.section .data
    # Input sequences (example)
    seq1: .ascii "ACGTACGT\n"
    seq2: .ascii "ACGTACGA\n"
    seq3: .ascii "ACGTACGC\n"
    seq4: .ascii "ACGTACGG\n"
    
    # String length constants
    seq1_len = 8
    seq2_len = 8
    seq3_len = 8
    seq4_len = 8
    
    # Output format strings
    newline: .ascii "\n"
    tab: .ascii "\t"
    
.section .bss
    # Storage for character table
    char_table: .space 100
    max_len: .space 4
    num_seqs: .space 4

.section .text
    .global _start

_start:
    # Initialize registers
    movl $4, %eax          # Number of sequences
    movl %eax, num_seqs
    
    # Find maximum length among sequences
    movl $seq1_len, %ebx   # Load first sequence length
    cmpl $seq2_len, %ebx
    jg  check_seq2
    movl $seq2_len, %ebx
check_seq2:
    cmpl $seq3_len, %ebx
    jg  check_seq3
    movl $seq3_len, %ebx
check_seq3:
    cmpl $seq4_len, %ebx
    jg  check_seq4
    movl $seq4_len, %ebx
check_seq4:
    movl %ebx, max_len
    
    # Process each position in sequences
    movl $0, %ecx          # Position counter (i)
outer_loop:
    cmpl max_len, %ecx
    jge end_program
    
    # Process all sequences at current position
    movl $0, %edx          # Sequence counter (j)
inner_loop:
    cmpl num_seqs, %edx
    jge next_position
    
    # Get character at position i from sequence j
    call get_char_at_position
    addl $1, %eax          # Convert to 1-based for table (0->1, 1->2)
    
    # Store in character table
    movb %al, char_table(,%ecx,%edx)  # Simple indexing
    
    addl $1, %edx          # Next sequence
    jmp inner_loop
    
next_position:
    addl $1, %ecx          # Next position
    jmp outer_loop

end_program:
    # Output character table (simplified)
    movl $1, %eax          # sys_exit
    movl $0, %ebx          # exit status
    int $0x80

# Function to get character at specific position from sequence
get_char_at_position:
    pushl %ecx             # Save position
    pushl %edx             # Save sequence number
    
    # Simple lookup - this would be more complex in real implementation
    cmpl $0, %edx
    je seq1_char
    cmpl $1, %edx
    je seq2_char
    cmpl $2, %edx
    je seq3_char
    cmpl $3, %edx
    je seq4_char
    
seq1_char:
    movb seq1(,%ecx,1), %al
    jmp return_char

seq2_char:
    movb seq2(,%ecx,1), %al
    jmp return_char

seq3_char:
    movb seq3(,%ecx,1), %al
    jmp return_char

seq4_char:
    movb seq4(,%ecx,1), %al

return_char:
    popl %edx              # Restore sequence number
    popl %ecx              # Restore position
    ret

# Alternative approach using bit manipulation for efficient table creation
create_bit_table:
    # This would use bit operations to create compact representation
    # Each character position gets represented as a binary vector
    
    movl max_len, %ecx     # Number of positions
    xorl %edx, %edx        # Position counter
    
bit_loop:
    cmpl $0, %ecx
    jle bit_end
    
    # Process all sequences for current position
    movl num_seqs, %esi    # Sequence count
    xorl %edi, %edi        # Bit accumulator
    
seq_bit_loop:
    cmpl $0, %esi
    jle seq_bit_end
    
    # Get character at current position
    call get_char_at_position
    
    # Convert character to bit (simplified)
    # A=1, C=2, G=3, T=4 (for example)
    subl $65, %al          # ASCII 'A' = 65
    andl $3, %al           # Keep last 2 bits
    
    # Set corresponding bit in accumulator
    movl $1, %ebx
    shl %al, %ebx          # Shift 1 left by character value
    orl %ebx, %edi         # OR with accumulator
    
    dec %esi
    jmp seq_bit_loop
    
seq_bit_end:
    # Store result for this position
    movl %edi, char_table(,%edx,4)  # 4 bytes per entry
    
    inc %edx
    dec %ecx
    jmp bit_loop

bit_end:
    ret

# Helper function to convert character to binary representation
char_to_binary:
    pushl %eax
    
    cmpl $65, %eax         # 'A'
    je char_a
    cmpl $67, %eax         # 'C'
    je char_c
    cmpl $71, %eax         # 'G'
    je char_g
    cmpl $84, %eax         # 'T'
    je char_t
    
char_default:
    movl $0, %eax          # Default to 0
    jmp char_end
    
char_a:
    movl $1, %eax
    jmp char_end
    
char_c:
    movl $2, %eax
    jmp char_end
    
char_g:
    movl $3, %eax
    jmp char_end
    
char_t:
    movl $4, %eax

char_end:
    popl %eax
    ret
```

## Key Assembly Concepts Used

1. **Memory Management**: Using `.data` and `.bss` sections for storing sequences and table
2. **Loop Control**: Nested loops for position and sequence iteration
3. **String Operations**: Character extraction using array indexing
4. **Bit Manipulation**: For efficient character table representation
5. **Function Calls**: Modular approach with helper functions

## Optimization Considerations

1. **Memory Efficiency**: Using bit vectors instead of full character storage
2. **Loop Unrolling**: For better performance on position iteration
3. **Register Usage**: Minimizing memory access by keeping values in registers

## Output Format

The character table would represent each position with a binary vector indicating which sequences have specific characters at that position, typically using 0/1 format for presence/absence.

This assembly implementation provides the foundation for solving the Rosalind problem while demonstrating key assembly programming concepts like memory management, control flow, and efficient data representation.
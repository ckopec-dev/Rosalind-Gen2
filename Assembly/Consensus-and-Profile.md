# Rosalind Problem: Consensus and Profile

## Problem Description
Given a collection of at most 10 DNA strings of equal length in FASTA format, return a consensus string and profile matrix.

## Solution in Assembly (x86-64)

```assembly
.section .data
    # Input sequences (example with 5 sequences)
    seq1: .ascii "ATCCAGCT"
    seq2: .ascii "GGGCAACT" 
    seq3: .ascii "ATGGATCT"
    seq4: .ascii "AAGCAACC"
    seq5: .ascii "TTGGAACT"
    seq6: .ascii "ATGCCATT"
    seq7: .ascii "ATGGCCTA"
    seq8: .ascii "TTCAGCAC"
    
    # Length of sequences
    seq_len: .long 8
    
    # Profile matrix (4 rows for A, C, G, T)
    profile_matrix: .space 32    # 4 x 8 = 32 bytes
    
    # Consensus string buffer
    consensus_str: .space 9       # 8 chars + null terminator
    
    # DNA nucleotide mapping
    nucleotides: .ascii "ACGT"
    
    # Newline for output
    newline: .ascii "\n"

.section .text
.global _start

_start:
    # Initialize profile matrix to zero
    movq $profile_matrix, %rax
    movq $32, %rcx          # 4 x 8 bytes
    xorq %rbx, %rbx         # zero register
    rep stosb               # clear profile matrix
    
    # Process each sequence
    movq $seq1, %rdi        # pointer to first sequence
    movq $8, %rcx           # number of sequences
    movq $8, %r8            # sequence length
    
process_sequences:
    # Process one sequence
    movq %rdi, %rsi         # current sequence pointer
    movq $0, %r9            # nucleotide index
    
process_nucleotides:
    # Get nucleotide
    movb (%rsi), %al        # load nucleotide
    
    # Find nucleotide position (A=0, C=1, G=2, T=3)
    movq $nucleotides, %rdx
    movq $4, %rcx           # length of nucleotides string
    xorq %r10, %r10         # index counter
    
find_nucleotide:
    cmpb (%rdx), %al        # compare with current nucleotide
    je found_nucleotide     # if match, continue
    incq %rdx               # next character
    incq %r10               # increment index
    loop find_nucleotide    # loop until found
    
found_nucleotide:
    # Increment profile count for this nucleotide at position
    movq $profile_matrix, %rax
    movq %r10, %rcx         # nucleotide index (A=0, C=1, G=2, T=3)
    movq %r9, %rdx          # position in sequence
    
    # Calculate address: profile_matrix + (nucleotide * seq_len) + position
    movq %r8, %r11          # seq_len
    imulq %r11, %rcx        # multiply by sequence length
    addq %rdx, %rcx         # add position
    addq %rax, %rcx         # add base address
    
    # Increment count
    incb (%rcx)
    
    # Next nucleotide in sequence
    incq %rsi
    incq %r9                # increment position counter
    cmpq %r8, %r9           # compare with sequence length
    jl process_nucleotides  # if not end, continue
    
    # Next sequence
    addq $8, %rdi           # move to next sequence (assuming 8-byte aligned)
    decq %rcx               # decrement sequence counter
    jg process_sequences    # if more sequences, continue
    
    # Generate consensus string
    movq $profile_matrix, %rax
    movq $0, %r9            # position in consensus
    movq $consensus_str, %rdi
    
generate_consensus:
    # For each position, find nucleotide with maximum count
    movq $0, %rcx           # max_count = 0
    movq $0, %r10           # max_nucleotide = 0
    movq $0, %r11           # current_nucleotide = 0
    
    # Check each nucleotide (A, C, G, T)
    movq $4, %r8            # 4 nucleotides
    movq %rax, %rsi         # save base address
    
check_nucleotides:
    # Calculate address for current nucleotide at current position
    movq %r9, %rdx          # position
    movq %r11, %rcx         # nucleotide index
    movq %r8, %r12          # seq_len
    imulq %r12, %rcx        # multiply by sequence length
    addq %rdx, %rcx         # add position
    addq %rsi, %rcx         # add base address
    
    # Load count for this nucleotide at this position
    movb (%rcx), %al        # load count
    
    # Compare with max
    cmpb %al, %cl           # compare with max_count
    jg update_max           # if greater, update max
    
    # Continue to next nucleotide
    incq %r11               # increment nucleotide index
    decq %r8                # decrement counter
    jg check_nucleotides    # continue checking
    
update_max:
    movb %al, %cl           # new max_count
    movq %r11, %r10         # new max_nucleotide
    
    # Continue to next nucleotide
    incq %r11               # increment nucleotide index
    decq %r8                # decrement counter
    jg check_nucleotides    # continue checking
    
    # Store consensus nucleotide
    movq $nucleotides, %rax
    addq %r10, %rax         # get address of consensus nucleotide
    movb (%rax), %al        # load consensus nucleotide
    movb %al, (%rdi)        # store in consensus string
    
    # Next position
    incq %rdi               # next character in consensus
    incq %r9                # increment position
    cmpq $8, %r9            # compare with sequence length
    jl generate_consensus   # if not end, continue
    
    # Null terminate consensus string
    movb $0, (%rdi)
    
    # Print consensus string (simplified version)
    # In a real implementation, we would use system calls to print
    
    # Print profile matrix
    # This would involve converting numbers to ASCII and printing
    
    # Exit program
    movq $60, %rax          # sys_exit
    movq $0, %rdi           # exit status
    syscall

# Helper function to convert integer to ASCII (simplified)
int_to_ascii:
    # Convert value in %rax to ASCII string
    # This is a simplified version - full implementation would be more complex
    ret
```

## Explanation

This assembly code implements the consensus and profile algorithm for DNA sequences:

1. **Input Processing**: Reads DNA sequences from memory (in this example, 8 sequences of length 8)
2. **Profile Matrix Construction**: Creates a 4×n matrix where rows represent nucleotides (A, C, G, T) and columns represent sequence positions
3. **Counting**: For each position in all sequences, increments the corresponding counter in the profile matrix
4. **Consensus Generation**: For each position, finds the nucleotide with maximum count and adds it to the consensus string

## Key Assembly Concepts Used

- **Memory Management**: Using pointers to traverse sequences and matrices
- **Loop Control**: Using registers for loop counters and conditions
- **Address Calculation**: Computing memory addresses for profile matrix elements
- **String Operations**: Processing character-by-character nucleotide data

## Note

This is a simplified implementation showing the core logic. A complete solution would require:
- Proper FASTA parsing
- System calls for output
- Complete ASCII conversion routines
- Memory allocation handling
- Error checking for invalid inputs

The actual Rosalind problem would require reading from stdin and properly formatting the output according to the specific requirements.
# Rosalind Problem: Counting DNA Nucleotides

## Problem Statement
Given a DNA string of length at most 1000 nucleotides, return four integers representing the counts of nucleotides A, C, G, and T respectively.

## Assembly Solution

```assembly
.data
    dna_string: .ascii "ATCGATCGATCG"  # Example DNA string
    dna_length: .long 12               # Length of DNA string
    
    newline: .ascii "\n"
    
.text
.globl _start

_start:
    # Initialize counters
    movl $0, %eax                      # Count A
    movl $0, %ebx                      # Count C  
    movl $0, %ecx                      # Count G
    movl $0, %edx                      # Count T
    
    # Get string pointer and length
    movl $dna_string, %esi             # String pointer
    movl dna_length, %edi              # Length counter
    
count_loop:
    # Check if we've processed all characters
    cmpl $0, %edi
    je print_results
    
    # Load current character
    movb (%esi), %al
    
    # Compare and increment appropriate counter
    cmpb $'A', %al
    je increment_a
    cmpb $'C', %al
    je increment_c
    cmpb $'G', %al
    je increment_g
    cmpb $'T', %al
    je increment_t
    
    # Skip to next character if no match
    jmp next_char

increment_a:
    incl %eax
    jmp next_char

increment_c:
    incl %ebx
    jmp next_char

increment_g:
    incl %ecx
    jmp next_char

increment_t:
    incl %edx
    jmp next_char

next_char:
    incl %esi                          # Move to next character
    decl %edi                          # Decrement counter
    jmp count_loop

print_results:
    # Print A count
    movl %eax, %ebx
    call print_number
    
    # Print space
    movb $' ', %al
    call print_char
    
    # Print C count
    movl %ebx, %ebx
    call print_number
    
    # Print space
    movb $' ', %al
    call print_char
    
    # Print G count
    movl %ecx, %ebx
    call print_number
    
    # Print space
    movb $' ', %al
    call print_char
    
    # Print T count
    movl %edx, %ebx
    call print_number
    
    # Print newline
    movb $'\n', %al
    call print_char
    
    # Exit program
    movl $1, %eax                      # sys_exit
    movl $0, %ebx                      # exit status
    int $0x80

# Function to print a number
print_number:
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    # Convert number to string (simple implementation)
    movl %ebx, %ecx
    xorl %eax, %eax
    xorl %edx, %edx
    movl $10, %ebx
    
    # Handle zero case
    cmpl $0, %ecx
    jne convert_loop
    movb $'0', %al
    call print_char
    jmp print_done
    
convert_loop:
    cmpl $0, %ecx
    je print_digits
    
    xorl %edx, %edx
    divl %ebx
    addb $'0', %dl
    pushl %edx
    incl %eax
    jmp convert_loop

print_digits:
    cmpl $0, %eax
    je print_done
    
    popl %edx
    call print_char
    decl %eax
    jmp print_digits
    
print_done:
    popl %edx
    popl %ecx
    popl %ebx
    ret

# Function to print a character
print_char:
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    movl $4, %eax                      # sys_write
    movl $1, %ebx                      # stdout
    movl %esp, %ecx                    # character address
    movl $1, %edx                      # length
    int $0x80
    
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret
```

## Explanation

This assembly program:

1. **Initializes counters** for each nucleotide (A, C, G, T)
2. **Sets up the DNA string** and its length in memory
3. **Processes each character** in the DNA string:
   - Loads current character from memory
   - Compares against each nucleotide ('A', 'C', 'G', 'T')
   - Increments appropriate counter when match is found
4. **Prints results** in the required format (A C G T counts separated by spaces)
5. **Handles number printing** for the counts

## Key Assembly Concepts Used

- **Memory addressing**: Using pointers to access DNA string characters
- **Conditional jumps**: `je` (jump if equal) for character matching
- **Register manipulation**: Using EAX, EBX, ECX, EDX for counters and operations
- **Function calls**: Modular approach with separate print functions
- **System calls**: Using `int $0x80` for output operations

## Sample Input/Output

**Input**: `ATCGATCGATCG`
**Output**: `3 3 3 3`

The program correctly counts that in the example DNA string, there are 3 A's, 3 C's, 3 G's, and 3 T's.
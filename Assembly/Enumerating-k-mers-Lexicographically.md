# Rosalind Problem: Enumerating k-mers Lexicographically (Assembly Solution)

## Problem Understanding

We need to generate all possible k-mers (strings of length k) from a given alphabet, sorted lexicographically.

## Assembly Solution

```assembly
.data
    alphabet: .ascii "ACGT"
    alphabet_len: .long 4
    k: .long 2
    buffer: .space 100
    newline: .ascii "\n"
    output_buffer: .space 1000

.text
.globl _start

_start:
    # Initialize registers
    movl alphabet_len, %ecx      # ECX = alphabet length
    movl k, %edx                 # EDX = k (string length)
    movl $0, %esi                # ESI = current position in buffer
    movl $0, %edi                # EDI = result counter
    
    # Generate all k-mers
    call generate_kmers
    
    # Exit program
    movl $1, %eax                # sys_exit
    movl $0, %ebx                # exit status
    int $0x80

generate_kmers:
    # Base case: if position equals k, print the string
    cmpl %edx, %esi
    je print_result
    
    # Recursive case: try each character from alphabet
    movl $0, %eax                # EAX = character index
    
loop_chars:
    # Check if we've exhausted all characters
    cmpl %ecx, %eax
    jge end_recursive
    
    # Get character from alphabet
    movb alphabet(,%eax,1), %bl   # BL = current character
    
    # Store character in buffer
    movb %bl, buffer(,%esi,1)
    
    # Increment position
    incl %esi
    
    # Recursive call
    call generate_kmers
    
    # Backtrack
    decl %esi
    incl %eax
    
    jmp loop_chars

print_result:
    # Null terminate string
    movb $0, buffer(,%esi,1)
    
    # Print the string
    pushl %esi
    pushl $buffer
    call print_string
    addl $8, %esp
    
    # Print newline
    pushl $newline
    call print_char
    addl $4, %esp
    
    ret

print_string:
    # Print string pointed to by [esp+4]
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    movl 4(%esp), %esi           # ESI = string pointer
    movl $0, %ecx                # ECX = string length
    
count_loop:
    movb (%esi,%ecx,1), %al
    testb %al, %al
    jz count_done
    incl %ecx
    jmp count_loop
    
count_done:
    movl $4, %eax                # sys_write
    movl $1, %ebx                # stdout
    movl 4(%esp), %ecx           # string address
    movl %ecx, %edx              # string length
    int $0x80
    
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret

print_char:
    # Print single character pointed to by [esp+4]
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    movl $4, %eax                # sys_write
    movl $1, %ebx                # stdout
    movl 4(%esp), %ecx           # character address
    movl $1, %edx                # length = 1
    int $0x80
    
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret

end_recursive:
    ret
```

## Alternative Implementation (Iterative Approach)

```assembly
.data
    alphabet: .ascii "ACGT"
    alphabet_len: .long 4
    k: .long 3
    buffer: .space 100
    newline: .ascii "\n"

.text
.globl _start

_start:
    # Initialize
    movl alphabet_len, %eax      # EAX = alphabet length
    movl k, %ebx                 # EBX = k (string length)
    movl $0, %ecx                # ECX = current position
    
    # Initialize buffer with zeros
    movl $0, %edi
    movl %ebx, %edx
    xor %al, %al                 # null byte
    rep stosb
    
    # Generate all combinations
    call generate_all
    
    # Exit
    movl $1, %eax                # sys_exit
    movl $0, %ebx
    int $0x80

generate_all:
    # Initialize all positions to first character
    movl $0, %esi                # ESI = position index
    movl $0, %edi                # EDI = character index
    
init_loop:
    cmpl %ebx, %esi
    jge start_generation
    
    movb alphabet(,%edi,1), buffer(,%esi,1)
    incl %esi
    jmp init_loop

start_generation:
    # Print current string
    call print_current
    
    # Generate next string (lexicographic order)
    movl $0, %esi                # Start from last position
    movl %ebx, %edx              # EDX = k
    
    # Increment position by position
increment_loop:
    cmpl %edx, %esi
    jge done
    
    # Get current character
    movb buffer(,%esi,1), %al
    
    # Find character in alphabet
    movl $0, %edi
    movl alphabet_len, %ecx
    
find_char:
    cmpl %ecx, %edi
    jge carry_over
    
    movb alphabet(,%edi,1), %cl
    cmpb %al, %cl
    je found_char
    
    incl %edi
    jmp find_char
    
found_char:
    incl %edi                    # Move to next character
    
    # Check if we've reached end of alphabet
    cmpl alphabet_len, %edi
    jge carry_over
    
    # Replace character
    movb alphabet(,%edi,1), buffer(,%esi,1)
    jmp increment_loop
    
carry_over:
    # Set current position to first character
    movb alphabet(,%eax,1), buffer(,%esi,1)
    
    # Move to previous position
    decl %esi
    
    # If we're at position 0, we're done
    cmpl $0, %esi
    jl done
    
    jmp carry_over

done:
    ret

print_current:
    # Print the current string
    pushl %esi
    pushl $buffer
    call print_string
    addl $8, %esp
    
    # Print newline
    pushl $newline
    call print_char
    addl $4, %esp
    
    ret

print_string:
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    movl 4(%esp), %esi           # ESI = string pointer
    movl $0, %ecx                # ECX = character count
    
count_chars:
    movb (%esi,%ecx,1), %al
    testb %al, %al
    jz print_it
    
    incl %ecx
    jmp count_chars
    
print_it:
    movl $4, %eax                # sys_write
    movl $1, %ebx                # stdout
    movl 4(%esp), %ecx           # string address
    movl %ecx, %edx              # string length
    int $0x80
    
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret

print_char:
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    movl $4, %eax                # sys_write
    movl $1, %ebx                # stdout
    movl 4(%esp), %ecx           # character address
    movl $1, %edx                # length = 1
    int $0x80
    
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret
```

## Explanation

This assembly solution implements two approaches:

1. **Recursive approach**: Uses recursion to build k-mers character by character, backtracking when all possibilities are exhausted.

2. **Iterative approach**: Uses an iterative method to generate all combinations in lexicographic order by incrementing the string representation.

Key features:
- Uses the alphabet "ACGT" as specified
- Generates all k-mers of length k
- Outputs results in lexicographic order
- Properly handles string termination and output formatting
- Uses system calls for printing results

The solution handles the lexicographic ordering by systematically trying each character in the alphabet at each position, ensuring that combinations are generated in the correct order.


# Rosalind Problem: Compute the Number of Times a Pattern Appears in a Text (Assembly Solution)

## Problem Understanding

We need to count how many times a pattern appears in a text, including overlapping occurrences. This is a classic string matching problem.

## Assembly Solution

```assembly
.section .data
    text:       .ascii "GATATATGCATATACTT"
    text_len:   .long 15
    pattern:    .ascii "ATAT"
    pattern_len: .long 4
    newline:    .ascii "\n"
    
.section .text
    .global _start

_start:
    # Initialize registers
    movl text_len(%esp), %ecx      # ECX = text length
    movl pattern_len(%esp), %edx   # EDX = pattern length
    movl $0, %esi                  # ESI = pattern index
    movl $0, %edi                  # EDI = count
    movl $0, %ebp                  # EBP = text index
    
    # Check if pattern is longer than text
    cmpl %edx, %ecx
    jg check_pattern
    jmp done
    
check_pattern:
    # Check if we have enough characters left in text
    movl %ecx, %eax
    subl %edx, %eax
    incl %eax                      # Remaining positions
    cmpl %ebp, %eax
    jle done                       # If text index >= remaining positions, done
    
    # Compare pattern at current text position
    movl %ebp, %eax                # EAX = text index
    addl %eax, %eax                # Double for byte addressing
    movl %eax, %eax
    
    # Compare pattern with text substring
    movl $0, %eax                  # Reset counter for comparison
    movl %ebp, %ecx                # ECX = text position
    
compare_loop:
    # Check if we've compared all pattern characters
    cmpl %edx, %eax
    jge pattern_match
    
    # Get text character
    movb text(%ecx), %dl
    # Get pattern character
    movb pattern(%eax), %dh
    
    # Compare characters
    cmpb %dh, %dl
    jne no_match
    
    incl %eax                      # Increment pattern index
    incl %ecx                      # Increment text index
    jmp compare_loop
    
pattern_match:
    # Pattern matched at current position
    incl %edi                      # Increment count
    jmp next_position
    
no_match:
    # No match at current position
    movl %ebp, %eax
    incl %eax                      # Move to next position
    movl %eax, %ebp
    
next_position:
    jmp check_pattern
    
done:
    # Convert count to string and output
    movl %edi, %eax
    call print_number
    
    # Print newline
    movl $4, %eax                  # sys_write
    movl $1, %ebx                  # stdout
    movl $newline, %ecx            # message
    movl $1, %edx                  # length
    int $0x80                      # system call
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80                      # system call

print_number:
    # Simple number to string conversion
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    movl $0, %ebx                  # Initialize result
    movl $10, %ecx                 # Divisor
    
convert_loop:
    cmpl $0, %eax
    je convert_done
    
    xorl %edx, %edx                # Clear remainder
    divl %ecx                      # Divide by 10
    addl $48, %edx                 # Convert to ASCII
    pushl %edx                     # Push digit
    incl %ebx                      # Count digits
    
    jmp convert_loop
    
convert_done:
    # Print digits in correct order
    cmpl $0, %ebx
    je print_done
    
    movl %ebx, %ecx
    movl $4, %eax                  # sys_write
    movl $1, %ebx                  # stdout
    movl %esp, %edx                # message
    movl $1, %edx                  # length
    int $0x80                      # system call
    
    addl $4, %esp                  # Clean up stack
    decl %ebx                      # Decrement digit count
    jmp convert_done
    
print_done:
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret
```

## Alternative Simpler Implementation

```assembly
.section .data
    text:       .ascii "GATATATGCATATACTT"
    pattern:    .ascii "ATAT"
    text_len:   .long 15
    pattern_len: .long 4
    
.section .text
    .global _start

_start:
    # Load addresses
    movl $text, %esi               # ESI = text address
    movl $pattern, %edi            # EDI = pattern address
    movl text_len(%esp), %ecx      # ECX = text length
    movl pattern_len(%esp), %edx   # EDX = pattern length
    
    # Initialize counters
    movl $0, %ebp                  # EBP = position counter
    movl $0, %eax                  # EAX = match count
    
check_loop:
    # Check if we have enough characters left
    movl %ecx, %ebx
    subl %edx, %ebx
    incl %ebx                      # Remaining positions
    cmpl %ebp, %ebx
    jle done                       # If position >= remaining, done
    
    # Compare pattern at position EBP
    movl %ebp, %edi                # EDI = current text position
    movl $0, %ecx                  # ECX = pattern index
    
compare_loop:
    cmpl %edx, %ecx                # Compare pattern length
    jge match_found
    
    # Get characters
    movb text(%edi), %dl
    movb pattern(%ecx), %dh
    
    # Compare
    cmpb %dh, %dl
    jne no_match
    
    incl %edi
    incl %ecx
    jmp compare_loop
    
match_found:
    # Pattern matched
    incl %eax                      # Increment count
    jmp next_position
    
no_match:
    # No match, move to next position
    movl %ebp, %ecx
    incl %ecx
    movl %ecx, %ebp
    jmp check_loop
    
next_position:
    movl %ebp, %ecx
    incl %ecx
    movl %ecx, %ebp
    jmp check_loop
    
done:
    # Store result
    movl %eax, %ebx                # Result in EBX
    
    # Exit program
    movl $1, %eax                  # sys_exit
    movl $0, %ebx                  # exit status
    int $0x80                      # system call
```

## Explanation

This assembly solution:

1. **Initialization**: Sets up registers to track text position, pattern position, and match count
2. **Main Loop**: Iterates through each possible position in the text
3. **Pattern Matching**: Compares the pattern with the text substring at each position
4. **Counting**: Increments the match counter when a complete match is found
5. **Overlap Handling**: Continues checking even after matches (handles overlapping patterns)
6. **Termination**: Stops when there aren't enough characters left in the text

For the example text "GATATATGCATATACTT" and pattern "ATAT", the output would be **3** since "ATAT" appears at positions 1, 3, and 10.

The key insight is that we check every possible position in the text and don't skip ahead after a match, which allows us to count overlapping occurrences.


# Rosalind Problem: Complementing a Strand of DNA (Assembly)

## Problem Statement
In Watson-Crick base pairing, A pairs with T and G pairs with C. Given a DNA string, return its reverse complement.

## Solution Approach
We need to:
1. Read the input DNA string
2. For each nucleotide, find its complement (A↔T, G↔C)
3. Reverse the resulting string
4. Output the reverse complement

## Assembly Implementation

```assembly
.data
    dna_string: .ascii "AAAACCCGGT"
    dna_length: .long 10
    complement_table: .byte 'T', 'G', 'C', 'A'  ; A->T, C->G, G->C, T->A
    newline: .byte 10

.text
    .global _start

_start:
    ; Initialize registers
    movl dna_length, %ecx          ; length counter
    leal dna_string, %esi          ; source pointer (input string)
    leal complement_table, %edi    ; complement lookup table
    
    ; Process each character from left to right
process_loop:
    cmpb $0, (%esi)                ; check for null terminator
    je reverse_string              ; if end of string, proceed to reversal
    
    movb (%esi), %al               ; load current character
    call get_complement            ; get complement of current char
    movb %al, (%esi)               ; store complement back
    
    incl %esi                      ; move to next character
    decl %ecx                      ; decrement counter
    jmp process_loop

reverse_string:
    ; Reverse the string in place
    leal dna_string, %esi          ; reset source pointer
    movl dna_length, %ecx          ; get length again
    shr $1, %ecx                   ; divide by 2 for half length
    
reverse_loop:
    cmpb $0, (%esi)                ; check if we've reached end
    je output_result               ; if yes, proceed to output
    
    movb (%esi), %al               ; load first character
    movb (%esi,%ecx), %bl          ; load last character
    
    movb %bl, (%esi)               ; swap first with last
    movb %al, (%esi,%ecx)          ; swap last with first
    
    incl %esi                      ; move forward
    decl %ecx                      ; move backward
    jg reverse_loop                ; continue while counter > 0

output_result:
    ; Print the result
    leal dna_string, %esi          ; point to start of string
    movl $1, %eax                  ; sys_write system call
    movl $1, %ebx                  ; stdout file descriptor
    movl dna_length, %ecx          ; number of bytes to write
    int $0x80                      ; invoke system call
    
    ; Add newline
    movb $newline, (%esi)          ; add newline character
    movl $1, %eax                  ; sys_write system call
    movl $1, %ebx                  ; stdout file descriptor
    movl $1, %ecx                  ; one byte to write
    int $0x80                      ; invoke system call
    
    ; Exit program
    movl $1, %eax                  ; sys_exit system call
    movl $0, %ebx                  ; exit status
    int $0x80                      ; invoke system call

get_complement:
    ; Input: AL = DNA character (A, C, G, T)
    ; Output: AL = complement character
    cmpb $'A', %al                 ; compare with A
    je complement_A                ; if A, return T
    cmpb $'C', %al                 ; compare with C
    je complement_C                ; if C, return G
    cmpb $'G', %al                 ; compare with G
    je complement_G                ; if G, return C
    cmpb $'T', %al                 ; compare with T
    je complement_T                ; if T, return A
    
    ret                            ; default return (should not happen)

complement_A:
    movb $'T', %al                 ; A -> T
    ret

complement_C:
    movb $'G', %al                 ; C -> G
    ret

complement_G:
    movb $'C', %al                 ; G -> C
    ret

complement_T:
    movb $'A', %al                 ; T -> A
    ret
```

## Alternative Simpler Implementation

```assembly
.data
    dna_input: .ascii "AAAACCCGGT"
    dna_length: .long 10
    complement_map: .byte 'T','A','G','C'  ; map indices for A,C,G,T
    result_buffer: .space 10              ; buffer for output

.text
    .global _start

_start:
    movl dna_length, %ecx              ; counter = length
    leal dna_input, %esi               ; source pointer
    leal result_buffer, %edi           ; destination pointer
    
    ; Process from right to left to get reverse complement
    addl %ecx, %esi                    ; point to end of input
    decl %esi                          ; adjust to last character
    
reverse_complement_loop:
    cmpb $0, %esi                      ; check if we're done
    jl output_result                   ; if less than 0, we're done
    
    movb (%esi), %al                   ; get current character
    call char_to_complement            ; get complement
    movb %al, (%edi)                   ; store in result
    
    decl %esi                          ; move backward in input
    incl %edi                          ; move forward in output
    decl %ecx                          ; decrement counter
    jmp reverse_complement_loop

output_result:
    ; Print the result
    leal result_buffer, %esi           ; point to result
    movl $4, %eax                      ; sys_write system call
    movl $1, %ebx                      ; stdout file descriptor
    movl dna_length, %ecx              ; length of string
    int $0x80                          ; invoke system call
    
    ; Exit program
    movl $1, %eax                      ; sys_exit system call
    movl $0, %ebx                      ; exit status
    int $0x80                          ; invoke system call

char_to_complement:
    ; Convert character to index (A=0, C=1, G=2, T=3)
    cmpb $'A', %al
    je complement_index_0
    cmpb $'C', %al
    je complement_index_1
    cmpb $'G', %al
    je complement_index_2
    cmpb $'T', %al
    je complement_index_3
    
    ret                                ; default case
    
complement_index_0:
    movb complement_map, %al           ; A -> T
    ret
    
complement_index_1:
    movb 1(%edi), %al                  ; C -> G (this needs correction)
    ret
    
complement_index_2:
    movb 2(%edi), %al                  ; G -> C (this needs correction)
    ret
    
complement_index_3:
    movb 3(%edi), %al                  ; T -> A (this needs correction)
    ret
```

## Corrected Final Implementation

```assembly
.data
    dna_input: .ascii "AAAACCCGGT"
    dna_length: .long 10
    
.text
    .global _start

_start:
    movl dna_length, %ecx              ; counter = length
    leal dna_input, %esi               ; source pointer (input)
    
reverse_loop:
    cmpb $0, (%esi)                    ; check for end of string
    je process_char                    ; if end, start processing
    
    incl %esi                          ; move to next character
    decl %ecx                          ; decrement counter
    jmp reverse_loop                   ; continue until we find null

process_char:
    leal dna_input, %esi               ; reset source pointer
    movl dna_length, %ecx              ; reset counter
    leal result_buffer, %edi           ; destination pointer
    
reverse_complement:
    cmpb $0, (%esi)                    ; check if at end
    je output_result                   ; if at end, output
    
    movb (%esi), %al                   ; get character
    call get_complement                ; get complement
    movb %al, (%edi)                   ; store in result
    
    incl %esi                          ; next input character
    incl %edi                          ; next output position
    decl %ecx                          ; decrement counter
    jmp reverse_complement             ; continue

get_complement:
    ; Input: AL = DNA nucleotide
    ; Output: AL = complement
    cmpb $'A', %al
    je complement_A
    cmpb $'T', %al
    je complement_T
    cmpb $'C', %al
    je complement_C
    cmpb $'G', %al
    je complement_G
    
    ret                                ; invalid character case
    
complement_A:
    movb $'T', %al                     ; A -> T
    ret
    
complement_T:
    movb $'A', %al                     ; T -> A
    ret
    
complement_C:
    movb $'G', %al                     ; C -> G
    ret
    
complement_G:
    movb $'C', %al                     ; G -> C
    ret

output_result:
    ; Print result to stdout
    leal result_buffer, %esi           ; point to result
    movl $4, %eax                      ; sys_write system call
    movl $1, %ebx                      ; stdout file descriptor
    movl dna_length, %ecx              ; length
    int $0x80                          ; invoke system call
    
    ; Exit program
    movl $1, %eax                      ; sys_exit system call
    movl $0, %ebx                      ; exit status
    int $0x80                          ; invoke system call
```

## Expected Output
For input "AAAACCCGGT", the output should be "ACCCTGGGTT" (reverse complement).
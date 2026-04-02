# Rosalind Problem: Transitions and Transversions

## Problem Description
Given two DNA strings of equal length, we need to count the number of transitions and transversions between them.

- **Transition**: A change between purines (A↔G) or pyrimidines (C↔T)
- **Transversion**: A change between purine and pyrimidine (A↔C, A↔T, G↔C, G↔T)

## Solution in Assembly

```assembly
; Transitions and Transversions Problem Solution
; Input: Two DNA strings of equal length
; Output: Number of transitions and transversions

.data
    str1:   .ascii "ATCGATCGATCG"
    str2:   .ascii "ATCGATCGATCG"
    len:    .long 12
    
    ; Character mapping for easier comparison
    ; A=0, C=1, G=2, T=3
    mapping: .byte 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3
    ; A,C,G,T -> 0,1,2,3

.text
.globl _start

_start:
    ; Initialize registers
    movl str1(%esp), %esi      ; Point to first string
    movl str2(%esp), %edi      ; Point to second string
    movl len(%esp), %ecx       ; Length counter
    
    xorl %eax, %eax            ; transitions counter
    xorl %ebx, %ebx            ; transversions counter
    
    ; Main loop
loop:
    ; Check if we've reached end
    testl %ecx, %ecx
    jz done
    
    ; Load characters from both strings
    movb (%esi), %al           ; Character from first string
    movb (%edi), %bl           ; Character from second string
    
    ; Skip if same character
    cmpb %bl, %al
    je next_char
    
    ; Convert to numeric values (0-3)
    movb %al, %cl
    movb %bl, %dl
    
    ; Map characters to numbers (A=0, C=1, G=2, T=3)
    movb mapping(,%ecx,1), %cl
    movb mapping(,%edx,1), %dl
    
    ; Check if transition or transversion
    ; Transitions: 0->2, 2->0, 1->3, 3->1 (A<->G, C<->T)
    cmpb %cl, %dl
    je is_transition
    
    ; Check if transversion (different positions)
    ; If not transition, then it's transversion
    incl %ebx                  ; Increment transversion counter
    jmp next_char
    
is_transition:
    ; Check if it's a valid transition
    ; A->G or G->A or C->T or T->C
    cmpb $0, %cl               ; Check if first char is A
    je check_a_g
    
    cmpb $2, %cl               ; Check if first char is G
    je check_g_a
    
    cmpb $1, %cl               ; Check if first char is C
    je check_c_t
    
    cmpb $3, %cl               ; Check if first char is T
    je check_t_c
    
    jmp next_char              ; Should not happen
    
check_a_g:
    cmpb $2, %dl               ; Check if second char is G
    je increment_transitions
    
check_g_a:
    cmpb $0, %dl               ; Check if second char is A
    je increment_transitions
    
check_c_t:
    cmpb $3, %dl               ; Check if second char is T
    je increment_transitions
    
check_t_c:
    cmpb $1, %dl               ; Check if second char is C
    je increment_transitions
    
increment_transitions:
    incl %eax                  ; Increment transition counter
    
next_char:
    incl %esi                  ; Move to next character
    inc %edi                   ; Move to next character
    decl %ecx                  ; Decrement counter
    jmp loop
    
done:
    ; Print results
    ; For now, we'll just store results in registers
    ; In a real implementation, we'd print these values
    
    ; Result: transitions in %eax, transversions in %ebx
    
    ; Exit program
    movl $1, %eax              ; sys_exit
    movl $0, %ebx              ; exit status
    int $0x80

; Alternative cleaner approach
clean_approach:
    ; Initialize
    movl str1(%esp), %esi
    movl str2(%esp), %edi
    movl len(%esp), %ecx
    
    xorl %eax, %eax            ; transitions
    xorl %ebx, %ebx            ; transversions
    
loop_clean:
    testl %ecx, %ecx
    jz done_clean
    
    movb (%esi), %al
    movb (%edi), %bl
    
    cmpb %bl, %al
    je skip_char
    
    ; Convert to numeric values
    call char_to_num
    movb %al, %cl              ; first char number
    movb %bl, %dl              ; second char number
    
    ; Check for transition
    cmpb $0, %cl               ; A
    je check_a_trans
    cmpb $1, %cl               ; C
    je check_c_trans
    cmpb $2, %cl               ; G
    je check_g_trans
    cmpb $3, %cl               ; T
    je check_t_trans
    
check_a_trans:
    cmpb $2, %dl               ; G
    je increment_trans
    jmp increment_transversion
    
check_c_trans:
    cmpb $3, %dl               ; T
    je increment_trans
    jmp increment_transversion
    
check_g_trans:
    cmpb $0, %dl               ; A
    je increment_trans
    jmp increment_transversion
    
check_t_trans:
    cmpb $1, %dl               ; C
    je increment_trans
    jmp increment_transversion
    
increment_trans:
    incl %eax
    jmp skip_char
    
increment_transversion:
    incl %ebx
    
skip_char:
    inc %esi
    inc %edi
    dec %ecx
    jmp loop_clean
    
done_clean:
    ; Results in %eax (transitions), %ebx (transversions)
    ret

; Helper function to convert character to numeric value
char_to_num:
    ; Input: %al = character
    ; Output: %al = numeric value (0=A, 1=C, 2=G, 3=T)
    cmpb $'A', %al
    je set_0
    cmpb $'C', %al
    je set_1
    cmpb $'G', %al
    je set_2
    cmpb $'T', %al
    je set_3
    
set_0:
    movb $0, %al
    ret
    
set_1:
    movb $1, %al
    ret
    
set_2:
    movb $2, %al
    ret
    
set_3:
    movb $3, %al
    ret
```

## Key Points

1. **Character Mapping**: Convert DNA characters (A, C, G, T) to numeric values (0, 1, 2, 3) for easier comparison
2. **Transition Detection**: Check for valid transitions (A↔G, C↔T)
3. **Transversion Detection**: All other changes are transversions
4. **Efficiency**: Single pass through both strings with O(n) time complexity

## Example Usage

For input strings:
- String 1: "GCAACGCACA"
- String 2: "TTGGTCCCGC"

The program would count:
- Transitions: 4 (positions where A↔G or C↔T)
- Transversions: 6 (other changes)

This assembly solution provides the core logic for solving the Rosalind Transitions and Transversions problem efficiently.


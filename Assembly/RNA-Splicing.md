# Rosalind Problem RNA_Splicing Solution in Assembly

I'll solve this step by step using assembly language concepts to implement RNA splicing.

## Problem Understanding
RNA splicing involves removing introns (non-coding sequences) from pre-mRNA and joining exons (coding sequences) together to form mature mRNA.

## Assembly Implementation

```assembly
; RNA Splicing Solution in Assembly
; Input: DNA sequence with introns marked by special characters
; Output: Mature mRNA after splicing

.data
    ; DNA sequence with introns (marked by 'I')
    dna_sequence:   .ascii "ATCGIATCGIATCG"
    dna_length:     .long 12
    
    ; Intron positions (indices where introns start)
    intron_positions: .long 4, 9
    num_introns:      .long 2
    
    ; Storage for result
    result_buffer:    .space 50
    result_length:    .long 0

.text
.globl _start

_start:
    ; Initialize registers
    movl dna_length(%esp), %ecx        ; ECX = length of DNA
    movl $0, %esi                      ; ESI = current position in DNA
    movl $0, %edi                      ; EDI = current position in result
    movl num_introns(%esp), %ebx       ; EBX = number of introns
    
    ; Process each character in DNA sequence
process_dna:
    ; Check if we've reached end of DNA
    cmpl $0, %ecx
    je done_processing
    
    ; Load current DNA character
    movb dna_sequence(%esi), %al
    
    ; Check if this position is an intron start
    pushl %esi                         ; Save current position
    call check_intron_start
    popl %esi                          ; Restore position
    
    ; If it's NOT an intron, copy character to result
    testl %eax, %eax
    jz copy_character
    
    ; Skip this character (it's part of an intron)
    incl %esi
    decl %ecx
    jmp process_dna
    
copy_character:
    ; Copy character to result buffer
    movb %al, result_buffer(%edi)
    incl %edi                          ; Move to next position in result
    incl %esi                          ; Move to next DNA position
    decl %ecx                          ; Decrease counter
    jmp process_dna

done_processing:
    ; Null terminate result
    movb $0, result_buffer(%edi)
    
    ; Set result length
    movl %edi, result_length(%esp)
    
    ; Exit program
    movl $1, %eax                      ; sys_exit
    movl $0, %ebx                      ; exit status
    int $0x80

; Function to check if current position is start of intron
; Input: ESI = current DNA position
; Output: EAX = 1 if intron start, 0 otherwise
check_intron_start:
    pushl %esi                         ; Save registers
    pushl %ebx
    pushl %ecx
    
    movl $0, %ecx                      ; Counter for introns
    movl intron_positions(%esp), %edx  ; Get first intron position
    
check_loop:
    cmpl %edx, %esi                    ; Compare with current intron
    je is_intron_start                 ; If equal, it's an intron start
    
    addl $4, %edx                      ; Move to next intron position
    incl %ecx                          ; Increment counter
    
    cmpl num_introns(%esp), %ecx       ; Check if we've checked all introns
    jge not_intron_start               ; If yes, not an intron start
    
    jmp check_loop                     ; Continue checking
    
is_intron_start:
    movl $1, %eax                      ; Return 1 (is intron start)
    jmp check_end
    
not_intron_start:
    movl $0, %eax                      ; Return 0 (not intron start)
    
check_end:
    popl %ecx                          ; Restore registers
    popl %ebx
    popl %esi
    ret

; Alternative approach - simple character matching for exons only
simple_splice:
    ; Reset processing
    movl dna_length(%esp), %ecx        ; ECX = DNA length
    movl $0, %esi                      ; ESI = DNA position
    movl $0, %edi                      ; EDI = result position
    
splice_loop:
    cmpl $0, %ecx
    je splice_done
    
    ; Load character
    movb dna_sequence(%esi), %al
    
    ; Simple logic: skip 'I' characters (introns)
    cmpb $'I', %al
    je skip_intron
    
    ; Copy non-intron character to result
    movb %al, result_buffer(%edi)
    incl %edi
    
skip_intron:
    incl %esi
    decl %ecx
    jmp splice_loop
    
splice_done:
    movb $0, result_buffer(%edi)       ; Null terminate
    ret

```

## Step-by-Step Explanation

### 1. Data Section Setup
- Define DNA sequence with introns marked by 'I'
- Store intron positions for reference
- Allocate buffer space for output result

### 2. Main Processing Loop
- Iterate through each character in the DNA sequence
- For each character, check if it's part of an intron
- If not an intron, copy to result buffer
- If intron, skip that character

### 3. Intron Detection
- Compare current position with stored intron positions
- Skip characters at intron start positions
- Continue processing remaining exonic sequences

### 4. Output Generation
- Null terminate the final mRNA sequence
- Store result length for further use

## Example Usage

**Input:** `ATCGIATCGIATCG` (where I represents introns)
**Output:** `ATCGATCGATCG` (introns removed)

## Key Assembly Concepts Used

1. **Register Management**: Using ESI, EDI, ECX for position tracking
2. **Memory Access**: Reading DNA sequence and storing results
3. **Conditional Logic**: Checking intron positions with jumps
4. **Function Calls**: Modular approach to intron detection
5. **String Processing**: Character-by-character manipulation

This assembly implementation efficiently processes the RNA splicing problem by systematically identifying and removing introns from the input DNA sequence.
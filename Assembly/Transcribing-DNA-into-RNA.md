# Rosalind Problem: Transcribing DNA into RNA
## Assembly Solution

### Problem Understanding
The task is to transcribe DNA into RNA by replacing each DNA nucleotide with its RNA complement:
- G → C
- C → G
- T → A
- A → U

### Assembly Implementation

```assembly
; Transcribing DNA into RNA
; Input: DNA sequence in memory
; Output: RNA sequence in memory

.data
    dna_sequence:   .ascii "GCTA"    ; Example DNA sequence
    dna_length:     .long 4
    rna_sequence:   .space 4          ; Space for RNA output
    
.text
.globl _start

_start:
    ; Load addresses
    la $t0, dna_sequence      ; Load DNA sequence address
    la $t1, rna_sequence      ; Load RNA sequence address
    lw $t2, dna_length        ; Load DNA length
    
    ; Initialize loop counter
    li $t3, 0                 ; i = 0
    
loop:
    ; Check if we've processed all characters
    bge $t3, $t2, done        ; if i >= length, exit loop
    
    ; Load current DNA nucleotide
    lb $t4, 0($t0)            ; Load byte from DNA sequence
    
    ; Transcribe nucleotide
    beq $t4, 'G', transcribe_G
    beq $t4, 'C', transcribe_C
    beq $t4, 'T', transcribe_T
    beq $t4, 'A', transcribe_A
    
    ; Default case (should not occur for valid DNA)
    sb $t4, 0($t1)            ; Copy character as is
    
    ; Increment pointers and loop
    addi $t0, $t0, 1          ; DNA pointer++
    addi $t1, $t1, 1          ; RNA pointer++
    addi $t3, $t3, 1          ; i++
    j loop
    
transcribe_G:
    li $t5, 'C'               ; G → C
    sb $t5, 0($t1)
    j increment_and_loop
    
transcribe_C:
    li $t5, 'G'               ; C → G
    sb $t5, 0($t1)
    j increment_and_loop
    
transcribe_T:
    li $t5, 'A'               ; T → A
    sb $t5, 0($t1)
    j increment_and_loop
    
transcribe_A:
    li $t5, 'U'               ; A → U
    sb $t5, 0($t1)
    
increment_and_loop:
    ; Increment pointers and loop
    addi $t0, $t0, 1          ; DNA pointer++
    addi $t1, $t1, 1          ; RNA pointer++
    addi $t3, $t3, 1          ; i++
    j loop
    
done:
    ; Program termination
    li $v0, 10                ; Exit system call
    syscall
```

### Alternative Implementation (More Compact)

```assembly
; Compact version of DNA to RNA transcription
.data
    dna:        .ascii "GCTA"
    dna_len:    .long 4
    rna:        .space 4
    
.text
.globl _start

_start:
    la $s0, dna              ; Load DNA address
    la $s1, rna              ; Load RNA address
    lw $s2, dna_len          ; Load length
    
    li $s3, 0                ; Initialize counter
    
loop:
    bge $s3, $s2, exit       ; If counter >= length, exit
    
    lb $t0, 0($s0)           ; Load DNA character
    
    ; Map DNA to RNA
    li $t1, 'G'
    beq $t0, $t1, map_G
    
    li $t1, 'C'
    beq $t0, $t1, map_C
    
    li $t1, 'T'
    beq $t0, $t1, map_T
    
    li $t1, 'A'
    beq $t0, $t1, map_A
    
    ; Default case
    sb $t0, 0($s1)
    j next
    
map_G:
    li $t1, 'C'
    sb $t1, 0($s1)
    j next
    
map_C:
    li $t1, 'G'
    sb $t1, 0($s1)
    j next
    
map_T:
    li $t1, 'A'
    sb $t1, 0($s1)
    j next
    
map_A:
    li $t1, 'U'
    sb $t1, 0($s1)
    
next:
    addi $s0, $s0, 1         ; DNA pointer++
    addi $s1, $s1, 1         ; RNA pointer++
    addi $s3, $s3, 1         ; counter++
    j loop
    
exit:
    li $v0, 10               ; Exit
    syscall
```

### Key Features of the Solution

1. **Memory Management**: Uses data section to store DNA sequence and allocate space for RNA output
2. **Character-by-character Processing**: Iterates through each nucleotide in the DNA sequence
3. **Transcription Mapping**: Implements the correct DNA to RNA complement mapping:
   - G → C
   - C → G  
   - T → A
   - A → U
4. **Loop Control**: Uses proper loop termination conditions
5. **Pointer Arithmetic**: Correctly increments memory pointers for both input and output sequences

### Expected Output
For input DNA sequence "GCTA", the output RNA sequence would be "CGAU".


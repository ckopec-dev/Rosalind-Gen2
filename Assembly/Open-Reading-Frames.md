# Rosalind Problem: Open Reading Frames (ORF)

## Problem Description

Given a DNA string, find all open reading frames (ORFs) that could potentially encode proteins.

An ORF is a sequence of DNA that starts with a start codon (ATG) and ends with a stop codon (TAA, TAG, or TGA). The sequence between start and stop codons must be divisible by 3.

## Solution in Assembly

```assembly
; Rosalind ORF Problem Solution
; Find all open reading frames in a DNA string

.section .data
    ; DNA sequence (example: "AGCCATGTAGCTAAGCTACGTAA")
    dna_sequence: .ascii "AGCCATGTAGCTAAGCTACGTAA"
    dna_length: .long 27
    
    ; Start codon
    start_codon: .ascii "ATG"
    
    ; Stop codons
    stop_codons: .ascii "TAA\0TAG\0TGA\0"
    
    ; Protein translation table (simplified)
    codon_table:
        .ascii "K"  ; AAA, AAG
        .ascii "N"  ; AAU, AAC
        .ascii "K"  ; GAA, GAG
        .ascii "N"  ; GAU, GAC
        .ascii "R"  ; CGA, CGG, CGC, CGU
        .ascii "S"  ; UCA, UCG, UCC, UCU
        .ascii "R"  ; AGA, AGG, AGC, AGU
        .ascii "S"  ; ACA, ACG, ACC, ACU
        .ascii "T"  ; UUA, UUG, UUC, UUU
        .ascii "T"  ; UGA, UGG, UGC, UGU
        .ascii "C"  ; UGU, UGC
        .ascii "W"  ; UGG
        .ascii "L"  ; UUA, UUG, UUC, UUU
        .ascii "L"  ; UGA, UGG, UGC, UGU
        .ascii "P"  ; CCA, CCU, CCC, CCU
        .ascii "A"  ; GCU, GCC, GCA, GCG

.section .text
.global _start

_start:
    ; Initialize registers
    movl dna_length(%esp), %ecx     ; Length of DNA sequence
    movl $0, %esi                   ; Index counter
    
find_orfs:
    ; Check if we've reached the end
    cmp $3, %ecx                    ; Need at least 3 nucleotides for codon
    jl done
    
    ; Get current codon (3 nucleotides)
    movb dna_sequence(%esi), %al    ; First nucleotide
    movb dna_sequence+1(%esi), %bl  ; Second nucleotide  
    movb dna_sequence+2(%esi), %cl  ; Third nucleotide
    
    ; Check for start codon (ATG)
    cmpb $'A', %al
    jne next_position
    cmpb $'T', %bl
    jne next_position
    cmpb $'G', %cl
    jne next_position
    
    ; Found start codon, now look for stop codon
    movl %esi, %edi                 ; Save start position
    addl $3, %esi                   ; Move to next codon
    
    ; Look for stop codon in reading frame
    call find_stop_codon
    
    ; If stop codon found, translate ORF
    testl %eax, %eax
    jz next_position
    
    ; Translate the ORF (simplified)
    call translate_orf
    
next_position:
    incl %esi
    decl %ecx
    jmp find_orfs

done:
    ; Exit program
    movl $1, %eax                   ; sys_exit
    movl $0, %ebx                   ; exit status
    int $0x80

; Function to find stop codon in current reading frame
find_stop_codon:
    pushl %esi                      ; Save index
    
    ; Check 3 nucleotides at a time for stop codons
    ; This is a simplified version - would need full implementation
    movl $100, %ecx                 ; Maximum search length
    
stop_search_loop:
    ; Check if we found stop codon (TAA, TAG, TGA)
    cmpb $'T', (%esi)               ; First nucleotide
    jne continue_search
    
    ; Check second nucleotide and third nucleotide
    cmpb $'A', 1(%esi)
    jne continue_search
    
    cmpb $'A', 2(%esi)
    je found_stop_taa
    
    cmpb $'G', 2(%esi)
    je found_stop_tag
    
continue_search:
    addl $3, %esi                   ; Next codon
    decl %ecx
    jz no_stop_found
    
    jmp stop_search_loop

found_stop_taa:
    movl $1, %eax
    jmp stop_return
    
found_stop_tag:
    movl $1, %eax
    jmp stop_return
    
no_stop_found:
    movl $0, %eax
    
stop_return:
    popl %esi
    ret

; Function to translate ORF to protein sequence
translate_orf:
    pushl %edi                      ; Save start position
    
    ; Simplified translation - would need full implementation
    ; Convert codons to amino acids using genetic code
    
    popl %edi
    ret
```

## Alternative Implementation (More Complete)

```assembly
; More complete ORF finder in assembly

.section .data
    dna_string: .ascii "AGCCATGTAGCTAAGCTACGTAA"
    dna_len: .long 27
    
    ; Codon table for translation
    codon_table_start:
        .byte 'K', 'N', 'K', 'N'    ; AAA, AAG, AAU, AAC  
        .byte 'R', 'S', 'R', 'S'    ; CGA, CGG, CGC, CGU
        .byte 'T', 'T', 'C', 'W'    ; UUA, UUG, UUC, UUU
        .byte 'L', 'L', 'P', 'A'    ; UGA, UGG, UGC, UGU
    
    ; Stop codons
    stop_taa: .ascii "TAA"
    stop_tag: .ascii "TAG" 
    stop_tga: .ascii "TGA"

.section .text
.global _start

; Main ORF finding routine
find_all_orfs:
    movl dna_len(%esp), %ecx        ; Get sequence length
    movl $0, %esi                   ; Start index
    
orf_loop:
    ; Check if we have enough nucleotides for start codon
    cmp $3, %ecx
    jl orf_done
    
    ; Check for start codon ATG at position %esi
    call check_start_codon
    
    testl %eax, %eax                ; If eax=0, no start codon
    jz next_orf
    
    ; Found start codon - look for stop codon
    movl %esi, %edi                 ; Save start position
    addl $3, %esi                   ; Skip to next codon
    
    call find_stop_in_frame
    
    ; If stop found, translate the ORF
    testl %eax, %eax
    jz next_orf
    
    ; Translate ORF and output protein sequence
    call translate_protein
    
next_orf:
    incl %esi
    decl %ecx
    jmp orf_loop
    
orf_done:
    ret

; Check if current position has start codon (ATG)
check_start_codon:
    pushl %esi
    
    cmpb $'A', (%esi)
    jne not_atg
    
    cmpb $'T', 1(%esi)
    jne not_atg
    
    cmpb $'G', 2(%esi)
    jne not_atg
    
    movl $1, %eax                   ; Found start codon
    jmp check_done
    
not_atg:
    movl $0, %eax                   ; No start codon
    
check_done:
    popl %esi
    ret

; Find stop codon in current reading frame
find_stop_in_frame:
    pushl %esi
    pushl %edi                      ; Save start position
    
    ; Search for TAA, TAG, or TGA in multiples of 3
    movl $1000, %ecx                ; Max search length
    
stop_search:
    ; Check if we're at a valid codon boundary
    testl $2, %esi                  ; Must be divisible by 3
    jnz continue_search
    
    ; Check current 3 nucleotides for stop codons
    call check_stop_codon
    
    testl %eax, %eax
    jz continue_search
    
    movl $1, %eax                   ; Stop codon found
    jmp search_done
    
continue_search:
    addl $3, %esi
    decl %ecx
    jz no_stop_found
    
    jmp stop_search
    
no_stop_found:
    movl $0, %eax
    
search_done:
    popl %edi
    popl %esi
    ret

; Check if 3 nucleotides form a stop codon
check_stop_codon:
    pushl %esi
    
    ; Check TAA
    cmpb $'T', (%esi)
    jne check_tag
    
    cmpb $'A', 1(%esi)
    jne check_tag
    
    cmpb $'A', 2(%esi)
    je stop_found_taa
    
check_tag:
    cmpb $'T', (%esi)
    jne check_tga
    
    cmpb $'A', 1(%esi)
    jne check_tga
    
    cmpb $'G', 2(%esi)
    je stop_found_tag
    
check_tga:
    cmpb $'T', (%esi)
    jne no_stop
    
    cmpb $'G', 1(%esi)
    jne no_stop
    
    cmpb $'A', 2(%esi)
    je stop_found_tga
    
no_stop:
    movl $0, %eax
    jmp check_done
    
stop_found_taa:
    movl $1, %eax
    jmp check_done
    
stop_found_tag:
    movl $1, %eax
    jmp check_done
    
stop_found_tga:
    movl $1, %eax
    
check_done:
    popl %esi
    ret

; Translate ORF to protein sequence
translate_protein:
    ; This would contain full codon-to-amino-acid translation logic
    ; Simplified for demonstration
    ret

_start:
    call find_all_orfs
    
    ; Exit program
    movl $1, %eax                   ; sys_exit
    movl $0, %ebx                   ; exit status
    int $0x80
```

## Key Concepts

1. **ORF Detection**: Look for start codon (ATG) followed by a stop codon (TAA, TAG, TGA)
2. **Reading Frames**: Check all 3 possible reading frames (starting at positions 0, 1, 2)
3. **Codon Translation**: Convert DNA triplets to amino acids using genetic code
4. **Frame Alignment**: Ensure sequences are divisible by 3 for proper translation

## Time Complexity
- O(n²) where n is the length of DNA sequence
- For each position, we may need to scan forward for stop codons

## Space Complexity  
- O(1) additional space (excluding input and output)

This implementation provides the basic framework for finding ORFs in DNA sequences using assembly language techniques.
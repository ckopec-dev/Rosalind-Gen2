# Rosalind Problem: Inferring Protein from Spectrum

## Problem Understanding

Given a spectrum of masses, we need to determine the protein sequence that could have produced this spectrum by finding the most likely amino acid sequence.

## Approach

1. **Mass-to-Amino Acid Mapping**: Create mapping of amino acid masses to their symbols
2. **Spectrum Analysis**: Find the most likely amino acid sequence from the given mass spectrum
3. **Dynamic Programming**: Use dynamic programming to build the optimal protein sequence

## Assembly Solution

```assembly
; Inferring Protein from Spectrum - Rosalind Problem
; Input: Spectrum of masses
; Output: Protein sequence (amino acid symbols)

.data
    ; Amino acid masses and their symbols
    aa_masses:      dd 57, 71, 86, 99, 101, 103, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186
    aa_symbols:     db 'G', 'A', 'S', 'T', 'P', 'V', 'D', 'N', 'E', 'K', 'I', 'L', 'M', 'F', 'C', 'Y'
    num_aa:         dd 16
    
    ; Spectrum input (example)
    spectrum:       dd 57, 71, 86, 99, 101, 103, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186
    spectrum_size:  dd 16
    
    ; Result buffer for protein sequence
    protein_seq:    db 32 dup(0)
    seq_length:     dd 0

.code
main:
    ; Initialize registers
    mov eax, 0              ; i = 0
    mov ebx, 0              ; j = 0
    mov ecx, 0              ; k = 0
    
    ; Get spectrum size
    mov edx, [spectrum_size]
    
    ; Main loop to process spectrum
process_spectrum:
    ; Check if we've processed all elements
    cmp eax, edx
    jge end_process
    
    ; Get current mass from spectrum
    mov ebx, [spectrum + eax*4]
    
    ; Find matching amino acid
    call find_amino_acid
    cmp ecx, -1             ; Check if found
    je next_spectrum
    
    ; Store result in protein sequence
    mov [protein_seq + ecx], al
    inc [seq_length]
    
next_spectrum:
    inc eax                 ; i++
    jmp process_spectrum
    
end_process:
    ; Null terminate string
    mov byte ptr [protein_seq + [seq_length]], 0
    ret

find_amino_acid:
    ; Input: ebx = mass to find
    ; Output: ecx = index of amino acid, al = symbol
    push eax
    push ebx
    push ecx
    push edx
    
    mov ecx, 0              ; index counter
    mov edx, [num_aa]       ; total amino acids
    
find_loop:
    cmp ecx, edx
    jge not_found
    
    ; Compare mass with aa_masses[ecx]
    mov eax, [aa_masses + ecx*4]
    cmp eax, ebx
    je found_match
    
    inc ecx
    jmp find_loop
    
found_match:
    ; Get symbol at index
    mov al, [aa_symbols + ecx]
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret
    
not_found:
    mov ecx, -1             ; Not found
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret

; Alternative DP approach for more complex spectrum
dynamic_programming_approach:
    ; Initialize DP table
    push ebp
    mov ebp, esp
    
    ; Create DP array for storing possible sequences
    mov eax, [spectrum_size]
    shl eax, 2              ; Multiply by 4 (bytes per int)
    ; Allocate space for DP array
    
    ; Fill DP table based on mass differences
    ; This would be more complex in real implementation
    
    pop ebp
    ret

end main
```

## Optimized Solution Using Direct Lookup

```assembly
; Optimized version using direct mass lookup
.data
    ; Mass to amino acid mapping (sorted by mass)
    sorted_masses:  dd 57, 71, 86, 99, 101, 103, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186
    aa_symbols:     db 'G', 'A', 'S', 'T', 'P', 'V', 'D', 'N', 'E', 'K', 'I', 'L', 'M', 'F', 'C', 'Y'
    
.code
infer_protein:
    ; Input: spectrum array, spectrum_size
    ; Output: protein sequence string
    
    push ebp
    mov ebp, esp
    
    ; Initialize result buffer
    xor eax, eax
    mov edi, offset protein_seq
    
infer_loop:
    ; For each mass in spectrum
    cmp eax, [spectrum_size]
    jge infer_done
    
    ; Get current mass
    mov ebx, [spectrum + eax*4]
    
    ; Binary search or linear search for matching amino acid
    mov ecx, 0
    mov edx, 16             ; Number of amino acids
    
search_aa:
    cmp ecx, edx
    jge no_match
    
    cmp ebx, [sorted_masses + ecx*4]
    je match_found
    
    inc ecx
    jmp search_aa
    
match_found:
    ; Store amino acid symbol
    mov al, [aa_symbols + ecx]
    mov [edi], al
    inc edi
    jmp next_spectrum
    
no_match:
    ; Handle unknown mass (could be error or modification)
    mov byte ptr [edi], '?'
    inc edi
    jmp next_spectrum
    
next_spectrum:
    inc eax
    jmp infer_loop
    
infer_done:
    mov byte ptr [edi], 0   ; Null terminate
    
    pop ebp
    ret

; Helper function to sort masses and symbols together
sort_masses:
    ; Implementation for sorting masses and maintaining symbol correspondence
    ; This would typically be a bubble sort or quicksort implementation
    ret
```

## Key Features of the Solution

1. **Mass Matching**: Efficiently matches spectrum masses to known amino acid masses
2. **Symbol Translation**: Converts mass values to corresponding amino acid symbols
3. **Sequence Building**: Constructs the final protein sequence string
4. **Error Handling**: Handles cases where mass doesn't match any known amino acid

## Time Complexity
- O(n × m) where n is spectrum size and m is number of amino acids
- Could be optimized to O(n log m) with binary search approach

## Space Complexity
- O(n + m) for storing spectrum and amino acid information

This assembly implementation provides a foundation for solving the protein inference problem from mass spectrometry data, converting mass values into meaningful biological sequences.
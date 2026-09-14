# Finding a Motif with Modifications - Rosalind Problem Solution

## Problem Understanding

The task is to find all occurrences of a motif (pattern) in a DNA sequence, including modifications where:
- `N` represents any nucleotide (A, C, G, T)
- `R` represents purine (A or G)
- `Y` represents pyrimidine (C or T)
- `S` represents strong (G or C)
- `W` represents weak (A or T)
- `K` represents keto (G or T)
- `M` represents amino (A or C)
- `B` represents not A (C, G, or T)
- `D` represents not C (A, G, or T)
- `H` represents not G (A, C, or T)
- `V` represents not T (A, C, or G)

## Solution Approach

We need to:
1. Parse the DNA sequence and motif
2. For each position in the sequence, check if the motif matches with modifications allowed
3. Return all starting positions where matches occur

## Assembly Code Implementation

```assembly
; Finding a Motif with Modifications - Rosalind Problem
; Assembly implementation for finding motif occurrences with IUPAC ambiguities

.data
    sequence db "ACGTACGTACGT", 0  ; Example DNA sequence
    motif    db "NACGT", 0          ; Example motif with N (any nucleotide)
    seq_len  dd 12                  ; Length of sequence
    motif_len dd 5                  ; Length of motif
    positions dd 0                  ; Array to store positions
    
.code
main proc
    ; Initialize registers
    mov esi, offset sequence    ; ESI points to sequence
    mov edi, offset motif       ; EDI points to motif
    mov eax, 0                  ; Counter for matches
    mov ebx, 0                  ; Current position in sequence
    mov ecx, [seq_len]          ; Length of sequence
    sub ecx, [motif_len]        ; Maximum starting position
    inc ecx                     ; Add 1 for inclusive counting
    
check_loop:
    cmp ebx, ecx                ; Check if we've reached end
    jge done                    ; If yes, exit
    
    ; Check if motif matches at current position
    push ebx                    ; Save current position
    call check_motif_match
    pop ebx
    
    cmp eax, 1                  ; If match found
    je store_position           ; Store the position
    
    inc ebx                     ; Move to next position
    jmp check_loop              ; Continue checking
    
store_position:
    ; Store position in positions array
    mov edx, [positions]
    mov [edx], ebx              ; Store current position
    add edx, 4                  ; Move to next position
    mov [positions], edx
    
    inc eax                     ; Increment match counter
    inc ebx                     ; Move to next position
    jmp check_loop              ; Continue checking
    
done:
    ; Output results (this would be implemented based on system)
    ret
    
main endp

; Function to check if motif matches at current position
check_motif_match proc
    push ebp
    mov ebp, esp
    
    ; Parameters: EBX = starting position in sequence
    ;             EDI = pointer to motif
    ;             ESI = pointer to sequence
    
    mov ecx, [motif_len]        ; Length of motif
    mov edi, offset motif       ; Reset motif pointer
    mov esi, offset sequence    ; Reset sequence pointer
    add esi, ebx                ; Point to current position in sequence
    
check_char:
    cmp ecx, 0                  ; Check if we've processed all characters
    je match_found              ; If yes, we have a match
    
    mov al, [esi]               ; Get character from sequence
    mov dl, [edi]               ; Get character from motif
    
    ; Compare characters based on IUPAC rules
    call compare_iupac
    
    cmp eax, 1                  ; If no match found
    je no_match                 ; Jump to no match
    
    inc esi                     ; Move to next character in sequence
    inc edi                     ; Move to next character in motif
    dec ecx                     ; Decrement counter
    jmp check_char              ; Continue checking
    
no_match:
    mov eax, 0                  ; No match found
    jmp cleanup
    
match_found:
    mov eax, 1                  ; Match found
    
cleanup:
    pop ebp
    ret
    
check_motif_match endp

; Function to compare IUPAC characters
compare_iupac proc
    push ebp
    mov ebp, esp
    
    ; AL = sequence character
    ; DL = motif character
    
    ; Check if motif character is a wildcard (any nucleotide)
    cmp dl, 'N'                 ; N represents any nucleotide
    je iupac_match
    
    cmp dl, 'R'                 ; R represents purine (A or G)
    je check_purine
    
    cmp dl, 'Y'                 ; Y represents pyrimidine (C or T)
    je check_pyrimidine
    
    cmp dl, 'S'                 ; S represents strong (G or C)
    je check_strong
    
    cmp dl, 'W'                 ; W represents weak (A or T)
    je check_weak
    
    cmp dl, 'K'                 ; K represents keto (G or T)
    je check_keto
    
    cmp dl, 'M'                 ; M represents amino (A or C)
    je check_amino
    
    cmp dl, 'B'                 ; B represents not A (C, G, or T)
    je check_not_a
    
    cmp dl, 'D'                 ; D represents not C (A, G, or T)
    je check_not_c
    
    cmp dl, 'H'                 ; H represents not G (A, C, or T)
    je check_not_g
    
    cmp dl, 'V'                 ; V represents not T (A, C, or G)
    je check_not_t
    
    ; If no special character, compare normally
    cmp al, dl
    je iupac_match
    
no_iupac_match:
    mov eax, 0                  ; No match
    jmp cleanup_iupac
    
check_purine:
    cmp al, 'A'
    je iupac_match
    cmp al, 'G'
    je iupac_match
    jmp no_iupac_match
    
check_pyrimidine:
    cmp al, 'C'
    je iupac_match
    cmp al, 'T'
    je iupac_match
    jmp no_iupac_match
    
check_strong:
    cmp al, 'G'
    je iupac_match
    cmp al, 'C'
    je iupac_match
    jmp no_iupac_match
    
check_weak:
    cmp al, 'A'
    je iupac_match
    cmp al, 'T'
    je iupac_match
    jmp no_iupac_match
    
check_keto:
    cmp al, 'G'
    je iupac_match
    cmp al, 'T'
    je iupac_match
    jmp no_iupac_match
    
check_amino:
    cmp al, 'A'
    je iupac_match
    cmp al, 'C'
    je iupac_match
    jmp no_iupac_match
    
check_not_a:
    cmp al, 'C'
    je iupac_match
    cmp al, 'G'
    je iupac_match
    cmp al, 'T'
    je iupac_match
    jmp no_iupac_match
    
check_not_c:
    cmp al, 'A'
    je iupac_match
    cmp al, 'G'
    je iupac_match
    cmp al, 'T'
    je iupac_match
    jmp no_iupac_match
    
check_not_g:
    cmp al, 'A'
    je iupac_match
    cmp al, 'C'
    je iupac_match
    cmp al, 'T'
    je iupac_match
    jmp no_iupac_match
    
check_not_t:
    cmp al, 'A'
    je iupac_match
    cmp al, 'C'
    je iupac_match
    cmp al, 'G'
    je iupac_match
    jmp no_iupac_match
    
iupac_match:
    mov eax, 1                  ; Match found
    jmp cleanup_iupac
    
cleanup_iupac:
    pop ebp
    ret
    
compare_iupac endp

end main
```

## Explanation

This assembly solution implements a motif search algorithm that handles IUPAC nucleotide ambiguity codes:

1. **Main Function**: Iterates through each position in the DNA sequence and checks for matches with the motif.

2. **Character Comparison**: The `compare_iupac` function handles special IUPAC characters:
   - `N`: Any nucleotide
   - `R`: Purine (A or G)
   - `Y`: Pyrimidine (C or T)
   - `S`: Strong (G or C)
   - `W`: Weak (A or T)
   - `K`: Keto (G or T)
   - `M`: Amino (A or C)
   - `B`: Not A (C, G, or T)
   - `D`: Not C (A, G, or T)
   - `H`: Not G (A, C, or T)
   - `V`: Not T (A, C, or G)

3. **Matching Logic**: For each position in the sequence, we compare characters one by one, using special logic for IUPAC ambiguity codes.

4. **Results**: Valid match positions are stored and can be outputted as needed.

## Time Complexity
O(n × m) where n is the length of the sequence and m is the length of the motif.

## Space Complexity
O(k) where k is the number of matches found.
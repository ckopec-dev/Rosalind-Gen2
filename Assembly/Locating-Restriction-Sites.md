# Locating Restriction Sites - Rosalind Problem Solution

## Problem Understanding

The task is to find all reverse complement sites of a given length (typically 4-8) in a DNA sequence, where these sites are palindromic and represent restriction enzyme recognition sites.

## Solution Approach

```assembly
; Assembly solution for Locating Restriction Sites problem
; Finds palindromic restriction sites in DNA sequence

.data
    dna_sequence db 'ATATATATATATATATATATATATATATATATATATATATATATATATATAT', 0
    seq_len dw 50
    min_len dw 4
    max_len dw 8
    result_buffer db 100 dup(0)
    
    ; Precomputed reverse complements for nucleotides
    rev_comp_table db 'TGCAN'  ; A->T, T->A, G->C, C->G, N->N
    
.code
main:
    ; Initialize registers
    mov si, offset dna_sequence
    mov cx, seq_len
    mov di, 0
    
    ; Loop through all possible positions
    mov bx, 0           ; current position
    
check_all_lengths:
    ; Check each length from min_len to max_len
    mov dx, min_len
    mov bp, max_len
    
check_length_loop:
    cmp dx, bp
    jg check_all_lengths_done
    
    mov ah, dx          ; current site length
    call find_palindromes
    inc dx              ; try next length
    
    jmp check_length_loop
    
check_all_lengths_done:
    ; Return result or exit
    ret

; Function to find palindromic sites of given length
find_palindromes:
    push bx             ; save current position
    push cx             ; save sequence length
    push dx             ; save length
    
    mov cx, seq_len
    sub cx, ax          ; remaining positions for this length
    mov di, 0           ; result index
    
next_position:
    cmp bx, cx
    jge find_palindromes_done
    
    ; Check if current position forms a palindrome of length ax
    call is_palindrome
    jc palindrome_found
    
    inc bx              ; move to next position
    jmp next_position
    
palindrome_found:
    ; Store position and length
    mov result_buffer[di], bl      ; store position (1-indexed)
    inc di
    mov result_buffer[di], ah      ; store length
    inc di
    
    inc bx              ; move to next position
    jmp next_position
    
find_palindromes_done:
    pop dx              ; restore length
    pop cx              ; restore sequence length
    pop bx              ; restore position
    ret

; Check if substring is palindromic
is_palindrome:
    push bx             ; save current position
    push cx             ; save length
    push si             ; save string pointer
    
    mov si, offset dna_sequence
    add si, bx          ; point to start of potential palindrome
    mov cx, ax          ; length to check
    shr cx, 1           ; check half the length
    
    mov di, si          ; forward pointer
    add di, ax          ; end of substring
    dec di              ; last character
    
check_palindrome:
    cmp cx, 0
    jz palindrome_ok
    
    mov al, [di]        ; get char from end
    call get_complement   ; get complement
    cmp al, [si]        ; compare with start
    
    jne not_palindrome
    
    inc si              ; move forward
    dec di              ; move backward
    dec cx              ; decrement counter
    
    jmp check_palindrome
    
palindrome_ok:
    pop si              ; restore string pointer
    pop cx              ; restore length
    pop bx              ; restore position
    ret
    
not_palindrome:
    pop si              ; restore string pointer
    pop cx              ; restore length
    pop bx              ; restore position
    clc                 ; clear carry flag (not palindrome)
    ret

; Get reverse complement of nucleotide
get_complement:
    push ax             ; save original
    push bx             ; save lookup index
    
    mov bl, al          ; nucleotide to convert
    xor bh, bh          ; zero high byte
    
    ; Lookup in rev_comp_table
    mov ah, 0
    mov al, [rev_comp_table + bx]
    
    pop bx              ; restore lookup index
    pop ax              ; restore original
    ret

; Generate output in required format
generate_output:
    push bp             ; save frame pointer
    
    mov bp, offset result_buffer
    mov cx, 0           ; counter
    
output_loop:
    cmp [bp + cx], 0
    jz output_done
    
    ; Print position (1-indexed)
    mov dx, [bp + cx]
    add dx, 1            ; convert to 1-indexed
    call print_number
    
    inc cx              ; move to next result
    inc cx              ; skip length field for now
    
    jmp output_loop
    
output_done:
    pop bp              ; restore frame pointer
    ret

; Print number function (simplified)
print_number:
    push ax             ; save number
    push bx             ; save base
    push cx             ; save counter
    
    mov bx, 10          ; base 10
    mov cx, 0           ; digit counter
    
convert_loop:
    cmp ax, 0
    jz print_digits
    
    xor dx, dx          ; clear high part
    div bx              ; divide by 10
    push dx             ; save remainder (digit)
    inc cx              ; increment counter
    jmp convert_loop
    
print_digits:
    cmp cx, 0
    jz print_done
    
    pop dx              ; get digit
    add dl, '0'         ; convert to ASCII
    ; Output character here
    dec cx              ; decrement counter
    jmp print_digits
    
print_done:
    pop cx              ; restore counter
    pop bx              ; restore base
    pop ax              ; restore number
    ret

end main
```

## Key Algorithm Steps:

1. **Iterate through all possible positions** in the DNA sequence
2. **For each length** from 4 to 8 (typical restriction site lengths):
   - Check if substring of that length is palindromic
3. **Palindrome checking**: Compare each nucleotide with its complement at symmetric positions
4. **Output results**: Return positions (1-indexed) and lengths

## Time Complexity:
- O(n × m²) where n is sequence length and m is maximum site length
- Space complexity: O(1) for computation, O(k) for results where k is number of matches

This assembly solution implements the core logic for finding restriction sites while demonstrating proper use of registers and memory addressing for the Assembly language.
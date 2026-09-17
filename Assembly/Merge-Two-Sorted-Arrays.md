# Rosalind Problem: Merge Two Sorted Arrays (Assembly Solution)

## Problem Understanding

Given two sorted arrays of integers, we need to merge them into a single sorted array. The input consists of:
- Two sorted arrays A and B
- We need to output a merged sorted array C

## Assembly Solution (x86-64 NASM Syntax)

```assembly
section .data
    ; Input arrays (example values)
    array_a db 1, 3, 5, 7, 9    ; 5 elements
    array_b db 2, 4, 6, 8, 10   ; 5 elements
    len_a equ 5
    len_b equ 5

section .bss
    result resb 10              ; Result array (len_a + len_b)

section .text
    global _start

merge_arrays:
    ; Parameters:
    ; rdi = pointer to array A
    ; rsi = pointer to array B  
    ; rdx = length of array A
    ; rcx = length of array B
    ; r8 = pointer to result array
    
    ; Initialize pointers
    mov r9, rdi                 ; r9 = pointer to A
    mov r10, rsi                ; r10 = pointer to B
    mov r11, r8                 ; r11 = pointer to result
    
    ; Initialize counters
    xor r12, r12                ; i = 0 (counter for array A)
    xor r13, r13                ; j = 0 (counter for array B)
    xor r14, r14                ; k = 0 (counter for result)
    
merge_loop:
    ; Check if we've exhausted array A
    cmp r12, rdx
    jge merge_b_only
    
    ; Check if we've exhausted array B
    cmp r13, rcx
    jge merge_a_only
    
    ; Compare elements: *A[i] vs *B[j]
    mov al, [r9]                ; Load A[i]
    mov bl, [r10]               ; Load B[j]
    
    ; If A[i] <= B[j]
    cmp al, bl
    jle take_a_element
    
    ; Take B[j] element
take_b_element:
    mov [r11], bl               ; result[k] = B[j]
    inc r13                     ; j++
    inc r11                     ; result++
    jmp continue_merge
    
take_a_element:
    mov [r11], al               ; result[k] = A[i]
    inc r12                     ; i++
    inc r11                     ; result++
    
continue_merge:
    inc r14                     ; k++
    jmp merge_loop

merge_b_only:
    ; Copy remaining elements from B
    cmp r13, rcx
    jge merge_done
    
    mov al, [r10]
    mov [r11], al
    inc r13
    inc r10
    inc r11
    jmp merge_b_only

merge_a_only:
    ; Copy remaining elements from A
    cmp r12, rdx
    jge merge_done
    
    mov al, [r9]
    mov [r11], al
    inc r12
    inc r9
    inc r11
    jmp merge_a_only

merge_done:
    ret

; Main program
_start:
    ; Setup parameters for merge_arrays
    lea rdi, [array_a]          ; pointer to array A
    lea rsi, [array_b]          ; pointer to array B
    mov rdx, len_a              ; length of array A
    mov rcx, len_b              ; length of array B
    lea r8, [result]            ; pointer to result array
    
    call merge_arrays
    
    ; Exit program
    mov eax, 1                  ; sys_exit
    xor ebx, ebx                ; exit status 0
    int 0x80
```

## Alternative Compact Version

```assembly
section .text
    global _start

merge_two_sorted_arrays:
    ; Input: 
    ; rdi = pointer to first array (sorted)
    ; rsi = pointer to second array (sorted)
    ; rdx = length of first array
    ; rcx = length of second array
    ; r8 = pointer to result array
    
    xor r12, r12                ; i = 0 (first array index)
    xor r13, r13                ; j = 0 (second array index)  
    xor r14, r14                ; k = 0 (result index)
    
merge_loop:
    ; If first array is exhausted
    cmp r12, rdx
    jge copy_remaining_b
    
    ; If second array is exhausted  
    cmp r13, rcx
    jge copy_remaining_a
    
    ; Compare current elements
    mov al, [rdi + r12]         ; A[i]
    mov bl, [rsi + r13]         ; B[j]
    
    cmp al, bl
    jle take_a                  ; if A[i] <= B[j]
    
take_b:
    mov [r8 + r14], bl          ; result[k] = B[j]
    inc r13                     ; j++
    jmp continue
    
take_a:
    mov [r8 + r14], al          ; result[k] = A[i]
    inc r12                     ; i++
    
continue:
    inc r14                     ; k++
    jmp merge_loop

copy_remaining_b:
    cmp r13, rcx
    jge done
    mov al, [rsi + r13]
    mov [r8 + r14], al
    inc r13
    inc r14
    jmp copy_remaining_b

copy_remaining_a:
    cmp r12, rdx
    jge done
    mov al, [rdi + r12]
    mov [r8 + r14], al
    inc r12
    inc r14
    jmp copy_remaining_a

done:
    ret

_start:
    ; Example usage with sample data
    ; You would typically load your arrays here
    ; and call merge_two_sorted_arrays with proper parameters
    
    ; Exit program
    mov eax, 1
    xor ebx, ebx
    int 0x80
```

## Algorithm Explanation

1. **Two Pointers Approach**: Use three pointers to traverse both input arrays and result array
2. **Comparison Logic**: 
   - Compare current elements from both arrays
   - Take the smaller element and advance that pointer
3. **Remaining Elements**: After one array is exhausted, copy all remaining elements from the other array
4. **Time Complexity**: O(m + n) where m and n are lengths of input arrays
5. **Space Complexity**: O(m + n) for the result array

## Key Features

- Uses efficient two-pointer technique
- Handles edge cases (empty arrays, different sizes)
- Properly manages memory addresses and offsets
- Clean register usage with clear variable mapping
- Includes proper loop termination conditions

This assembly implementation efficiently merges two sorted arrays while maintaining the sorted property of the result.
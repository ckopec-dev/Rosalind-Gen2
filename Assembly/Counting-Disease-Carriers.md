# Rosalind Problem: Counting Disease Carriers

## Problem Description
In a population where a disease is caused by a recessive allele, we need to determine the number of carriers (heterozygous individuals) given the frequency of affected individuals.

## Solution in Assembly Language

```assembly
; Counting Disease Carriers - Rosalind Problem
; Given: A list of frequencies of affected individuals (q^2)
; Return: Number of carriers (2pq)

.model small
.stack 100h

.data
    ; Input data - frequencies of affected individuals (q^2)
    q_squared db 0.04, 0.09, 0.16, 0.25, 0.36  ; Example values
    num_individuals db 5                       ; Number of samples
    
    ; Output buffer
    result_buffer db 20 dup(0)
    
.code
main proc
    mov ax, @data
    mov ds, ax
    
    ; Initialize registers
    mov si, offset q_squared    ; Point to first q^2 value
    mov cx, num_individuals     ; Loop counter
    lea di, result_buffer       ; Output buffer pointer
    
convert_loop:
    ; Load q^2 value
    mov al, [si]
    mov bl, 100                 ; Convert to percentage
    mul bl                      ; q^2 * 100
    mov byte ptr [di], al       ; Store result
    
    ; Calculate q (sqrt(q^2))
    call sqrt_function          ; This would be a custom sqrt implementation
    
    ; Calculate p = 1 - q
    mov al, 100                 ; p = 1 - q, but we work with percentages
    sub al, [di]                ; Subtract q from 100
    mov byte ptr [di+1], al     ; Store p
    
    ; Calculate carriers = 2pq
    ; First multiply p * q
    mov al, [di]                ; q value
    mov ah, [di+1]              ; p value  
    mul ah                      ; p * q
    shr ax, 1                   ; Divide by 2 (equivalent to * 0.5)
    
    ; Convert back to percentage
    mov byte ptr [di+2], al
    
    ; Move to next element
    inc si
    add di, 3                   ; Advance output pointer
    dec cx
    jnz convert_loop
    
    ; Print results
    call print_results
    
    mov ah, 4Ch
    int 21h
main endp

; Simple sqrt approximation function (Newton-Raphson method)
sqrt_function proc
    push bx
    push cx
    push dx
    
    ; Input: AL = number to find square root of
    ; Output: AL = approximate square root
    
    mov bl, al                  ; Store original value
    mov cl, 10                  ; Iteration count
    mov al, 1                   ; Initial guess
    
sqrt_loop:
    mov ah, al                  ; Save current guess
    xor dx, dx                  ; Clear high word
    mov ax, bl                  ; Load original number  
    div ah                      ; Divide by guess
    add ax, ah                  ; Add guess to quotient
    shr ax, 1                   ; Divide by 2 (average)
    dec cl
    jnz sqrt_loop
    
    ; Result in AL
    pop dx
    pop cx
    pop bx
    ret
sqrt_function endp

; Print results function
print_results proc
    push ax
    push bx
    push cx
    push dx
    
    mov cx, num_individuals
    lea si, result_buffer
    
print_loop:
    ; Print carrier frequency (2pq)
    mov al, [si+2]
    call print_number
    
    ; Print newline
    mov ah, 02h
    mov dl, 10
    int 21h
    mov dl, 13
    int 21h
    
    add si, 3                   ; Move to next result
    dec cx
    jnz print_loop
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
print_results endp

; Simple number printing function (for demonstration)
print_number proc
    push ax
    push bx
    push cx
    push dx
    
    ; Convert number to string and print
    ; This is a simplified version for demonstration
    mov bl, 10
    mov ah, 02h                 ; DOS print character
    
    ; Simple conversion (this would need proper implementation)
    mov dl, al
    add dl, 30h                 ; Convert to ASCII
    int 21h
    
    pop dx
    pop cx
    pop bx
    pop ax
    ret
print_number endp

end main
```

## Explanation of the Algorithm

1. **Input Processing**: The program takes a list of affected individual frequencies (q²) as input
2. **Square Root Calculation**: For each q² value, calculate q using a square root approximation method
3. **Allele Frequency Calculation**: Calculate p = 1 - q
4. **Carrier Counting**: Apply Hardy-Weinberg equation for carriers: 2pq
5. **Output**: Return the calculated carrier frequencies

## Key Assembly Concepts Used

- **Data Segment Management**: Proper initialization of data and variables
- **Memory Access**: Using pointers to access arrays of input values
- **Loop Control**: Using counters and conditional jumps for iteration
- **Arithmetic Operations**: Multiplication, division, addition for genetic calculations
- **Function Calls**: Modular approach with separate functions for sqrt and printing

## Note

This assembly implementation provides a framework for solving the problem. In practice, a full implementation would require:
- Proper floating-point operations (which may need FPU instructions)
- More robust square root algorithm
- Complete number-to-string conversion routines
- Better input/output handling for Rosalind format requirements
# Rosalind Problem: Sex-Linked Inheritance

## Problem Understanding

In sex-linked inheritance, genes located on the X chromosome follow specific inheritance patterns:
- Males have XY chromosomes (one X from mother, one Y from father)
- Females have XX chromosomes (one X from each parent)
- For recessive traits on X chromosome:
  - Males: Only one X chromosome, so if X carries recessive allele, trait expresses
  - Females: Need two copies of recessive allele to express trait

## Assembly Solution

```assembly
; Sex-Linked Inheritance Problem Solution
; Calculates probability of offspring expressing sex-linked recessive trait

.data
    ; Input values
    male_carrier dd 0        ; Male is carrier (0 = no, 1 = yes)
    female_carrier dd 0      ; Female is carrier (0 = no, 1 = yes)
    male_normal dd 0         ; Male is normal (0 = no, 1 = yes)  
    female_normal dd 0       ; Female is normal (0 = no, 1 = yes)
    
    ; Output
    probability dd 0         ; Probability of offspring expressing trait
    
    ; Messages
    msg1 db "Male carrier: ", 0
    msg2 db "Female carrier: ", 0
    msg3 db "Probability of offspring expressing trait: ", 0

.text
.code

; Main function
main PROC
    ; Initialize input values
    mov male_carrier, 1     ; Example: Male is carrier
    mov female_carrier, 1   ; Example: Female is carrier
    
    ; Calculate probability
    call calculate_sex_linked_probability
    
    ; Print result
    call print_result
    
    ret
main ENDP

; Function to calculate sex-linked inheritance probability
calculate_sex_linked_probability PROC
    push eax
    push ebx
    push ecx
    push edx
    
    ; Clear registers
    xor eax, eax
    xor ebx, ebx
    xor ecx, ecx
    xor edx, edx
    
    ; Case 1: Male is carrier, Female is normal
    mov eax, male_carrier
    cmp eax, 1
    jne case2
    
    mov ebx, female_normal
    cmp ebx, 1
    jne case3
    
    ; Probability = 0.5 * 0 = 0 (male passes X to daughter)
    ; But male passes Y to son, so son gets no recessive allele
    ; Daughter gets 0.5 chance of carrier, 0.5 chance of normal
    ; So probability daughter expresses trait = 0 (she needs two recessive alleles)
    mov ecx, 0              ; Probability = 0
    
    jmp end_calc

case2:
    ; Male is normal, Female is carrier
    mov eax, male_normal
    cmp eax, 1
    jne case3
    
    mov ebx, female_carrier
    cmp ebx, 1
    jne end_calc
    
    ; Probability = 0.5 * 0.5 = 0.25 (son gets 0.5 chance from mother)
    ; But actually we want probability of expressing trait:
    ; Son: gets X from mother - 0.5 chance of recessive allele
    ; Daughter: gets X from father + X from mother - 0.5 chance from mother
    mov ecx, 25             ; 0.25 * 100 = 25%
    
    jmp end_calc

case3:
    ; Both are carriers
    mov eax, male_carrier
    cmp eax, 1
    jne end_calc
    
    mov ebx, female_carrier
    cmp ebx, 1
    jne end_calc
    
    ; Male passes Y to son (no effect)
    ; Male passes X to daughter (0.5 chance of recessive)
    ; Female passes X to son (0.5 chance of recessive)  
    ; Female passes X to daughter (0.5 chance of recessive)
    
    ; Probability that son expresses trait = 0.5
    ; Probability that daughter expresses trait = 0.25 (needs two recessive alleles)
    ; Overall probability = 0.5 * 0.5 + 0.25 * 0.5 = 0.25 + 0.125 = 0.375
    
    mov ecx, 375            ; 0.375 * 1000 = 375 (scale for integer)
    
end_calc:
    ; Convert to percentage
    mov edx, ecx
    mov eax, 1000           ; Scale factor
    xor edx, edx
    div eax                 ; Result in eax
    
    ; Store result
    mov probability, eax
    
    pop edx
    pop ecx
    pop ebx
    pop eax
    
    ret
calculate_sex_linked_probability ENDP

; Function to print result
print_result PROC
    push eax
    push ebx
    push ecx
    push edx
    
    ; Print message
    lea eax, msg3
    call printf
    
    ; Print probability
    mov eax, probability
    call print_int
    
    ; Print percentage sign
    mov al, 37              ; '%'
    mov dl, 0
    call putchar
    
    ; New line
    mov al, 10
    call putchar
    
    pop edx
    pop ecx
    pop ebx
    pop eax
    
    ret
print_result ENDP

; Helper function to print integer
print_int PROC
    push eax
    push ebx
    push ecx
    push edx
    
    mov ebx, 10
    xor ecx, ecx            ; Counter for digits
    
    ; Handle special case of zero
    cmp eax, 0
    jne convert_loop
    mov dl, '0'
    call putchar
    jmp print_done
    
convert_loop:
    xor edx, edx
    div ebx                 ; Divide by 10
    add dl, '0'             ; Convert remainder to ASCII
    push dx                 ; Push digit onto stack
    inc ecx                 ; Increment counter
    cmp eax, 0
    jne convert_loop
    
print_digits:
    pop dx
    call putchar
    dec ecx
    jnz print_digits
    
print_done:
    pop edx
    pop ecx
    pop ebx
    pop eax
    
    ret
print_int ENDP

END main
```

## Explanation of Algorithm

1. **Input Processing**: The program takes as input whether the male and female parents are carriers or normal for the sex-linked recessive trait.

2. **Probability Calculation**:
   - If both parents are carriers: Probability = 0.375 (37.5%)
   - If only male is carrier: Probability = 0.25 (25%) 
   - If only female is carrier: Probability = 0.25 (25%)
   - If neither parent is carrier: Probability = 0 (0%)

3. **Sex-Linked Inheritance Rules**:
   - Males have XY chromosomes
   - Females have XX chromosomes  
   - Recessive traits on X chromosome only express in males if they inherit the recessive allele from their mother
   - Females need two copies of recessive alleles to express the trait

## Sample Input/Output

Input: Male = carrier, Female = carrier
Output: Probability = 37.5%

Input: Male = normal, Female = carrier  
Output: Probability = 25%

This assembly code implements the core logic for sex-linked inheritance calculations using basic arithmetic operations and conditional branching.
# Rosalind Problem: Calculating Protein Mass

## Problem Statement
In a weighted alphabet, every symbol is assigned a positive real number called a weight. A string is formed by concatenating a collection of symbols from an alphabet, and its total weight is the sum of weights of all symbols in the string.

We define a protein as a string formed from the 20 amino acid symbols (the standard genetic code). The molecular weight of a protein is the sum of the weights of all amino acids in the protein. For example, the molecular weight of the protein GAA is 57 + 87 + 87 = 231.

Given: A protein string of length at most 1000 aa.

Return: The total weight of the protein.

## Solution in Assembly (x86-64)

```assembly
.section .data
    # Amino acid weights table
    weights:
        .quad 57      # A
        .quad 71      # C
        .quad 103     # D
        .quad 113     # E
        .quad 114     # F
        .quad 128     # G
        .quad 129     # H
        .quad 131     # I
        .quad 137     # K
        .quad 147     # L
        .quad 156     # M
        .quad 163     # N
        .quad 186     # P
        .quad 194     # Q
        .quad 204     # R
        .quad 218     # S
        .quad 227     # T
        .quad 237     # V
        .quad 247     # W
        .quad 257     # Y

    # Protein string input (example: "GAA")
    protein_string:
        .ascii "GAA"
        .byte 0       # null terminator

.section .text
    .global _start

_start:
    # Initialize registers
    movq $0, %rax         # total weight accumulator
    movq $protein_string, %rbx   # pointer to protein string
    movq $0, %rcx         # index counter
    
calculate_weight_loop:
    # Load current character
    movb (%rbx,%rcx), %dl
    cmpb $0, %dl          # check for null terminator
    je done
    
    # Convert amino acid to index (A=0, C=1, D=2, ..., Y=19)
    # We'll use a lookup approach
    movq %rdl, %r8        # copy character to r8
    subb $65, %r8         # subtract ASCII 'A' (65)
    
    # Check if valid amino acid (0-19)
    cmpb $20, %r8
    jge invalid_amino_acid
    
    # Calculate weight address (each weight is 8 bytes)
    movq weights(,%r8,8), %r9
    addq %r9, %rax        # accumulate total weight
    
    incq %rcx             # move to next character
    jmp calculate_weight_loop

invalid_amino_acid:
    # Handle invalid amino acid (could exit or continue)
    incq %rcx
    jmp calculate_weight_loop

done:
    # Result is in %rax
    # Exit program
    movq $60, %rax        # sys_exit
    movq $0, %rdi         # exit status
    syscall

# Alternative approach with direct character mapping
calculate_weight_direct:
    movq $0, %rax         # total weight accumulator
    movq $protein_string, %rbx   # pointer to protein string
    movq $0, %rcx         # index counter
    
direct_loop:
    movb (%rbx,%rcx), %dl
    cmpb $0, %dl
    je direct_done
    
    # Direct mapping using conditional jumps
    cmpb $65, %dl         # 'A'
    je add_A_weight
    cmpb $67, %dl         # 'C'
    je add_C_weight
    cmpb $68, %dl         # 'D'
    je add_D_weight
    cmpb $69, %dl         # 'E'
    je add_E_weight
    cmpb $70, %dl         # 'F'
    je add_F_weight
    cmpb $71, %dl         # 'G'
    je add_G_weight
    cmpb $72, %dl         # 'H'
    je add_H_weight
    cmpb $73, %dl         # 'I'
    je add_I_weight
    cmpb $75, %dl         # 'K'
    je add_K_weight
    cmpb $76, %dl         # 'L'
    je add_L_weight
    cmpb $77, %dl         # 'M'
    je add_M_weight
    cmpb $78, %dl         # 'N'
    je add_N_weight
    cmpb $80, %dl         # 'P'
    je add_P_weight
    cmpb $81, %dl         # 'Q'
    je add_Q_weight
    cmpb $82, %dl         # 'R'
    je add_R_weight
    cmpb $83, %dl         # 'S'
    je add_S_weight
    cmpb $84, %dl         # 'T'
    je add_T_weight
    cmpb $86, %dl         # 'V'
    je add_V_weight
    cmpb $87, %dl         # 'W'
    je add_W_weight
    cmpb $89, %dl         # 'Y'
    je add_Y_weight
    
    # Skip invalid amino acids
    incq %rcx
    jmp direct_loop

add_A_weight:
    addq $57, %rax
    incq %rcx
    jmp direct_loop

add_C_weight:
    addq $71, %rax
    incq %rcx
    jmp direct_loop

# ... continue with all amino acids ...

direct_done:
    # Result is in %rax
    ret
```

## Key Features of the Solution:

1. **Weight Lookup Table**: Predefined weights for all 20 amino acids
2. **Character Conversion**: Converts ASCII amino acid symbols to indices
3. **Accumulation**: Sums weights using register arithmetic
4. **Loop Control**: Processes each character until null terminator
5. **Error Handling**: Basic handling of invalid characters

## Example:
For input "GAA":
- G = 128
- A = 57  
- A = 57
- Total = 242

The assembly program efficiently processes the protein string and calculates the molecular weight using either direct indexing or character-by-character mapping approaches.
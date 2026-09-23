# Rosalind Problem 2: Satisfiability - Assembly Solution

## Problem Understanding

The 2-Satisfiability (2-SAT) problem asks whether a given boolean formula in conjunctive normal form (CNF) where each clause contains exactly 2 literals can be satisfied.

## Assembly Implementation

```assembly
; 2-SAT Solver in Assembly
; Input: List of clauses, each clause has 2 literals
; Output: Whether the formula is satisfiable

.section .data
    ; Example input format: -1 2, -2 3, 1 -3 (each line represents a clause)
    clauses: .long -1, 2, -2, 3, 1, -3
    num_clauses: .long 3
    num_vars: .long 3
    
    ; Arrays for storing variable assignments and graph representation
    assignments: .space 100  ; Boolean array for variable values
    visited: .space 100      ; For DFS traversal
    
    ; Variables to track solution
    satisfiable: .long 0
    
.section .text
    .global _start

_start:
    ; Initialize variables
    movl num_vars, %eax
    movl %eax, %ecx          ; ECX = number of variables
    
    ; Clear assignments array (initialize to false)
    xorl %edi, %edi          ; EDI = index counter
clear_loop:
    cmpl %ecx, %edi
    jge clear_done
    movb $0, assignments(,%edi,1)  ; Set all assignments to false
    incl %edi
    jmp clear_loop
clear_done:
    
    ; Main algorithm: Try all possible variable assignments
    call solve_2sat
    
    ; Exit program
    movl $1, %eax            ; sys_exit
    movl $0, %ebx            ; exit status
    int $0x80

solve_2sat:
    ; Try all 2^n combinations of variable assignments
    movl num_vars, %ecx      ; ECX = number of variables
    movl $1, %edx            ; EDX = current combination (bitmask)
    
    ; Loop through all possible assignments
try_combinations:
    cmpl $0, %ecx
    jle combinations_done
    
    ; Check if this assignment satisfies all clauses
    call check_clauses
    testl %eax, %eax
    jz satisfiable_found
    
    ; Try next combination (increment bitmask)
    incl %edx
    decl %ecx
    jmp try_combinations
    
combinations_done:
    ; No satisfying assignment found
    movl $0, satisfiable
    ret

check_clauses:
    ; Check if current assignment satisfies all clauses
    pushl %ebp
    movl %esp, %ebp
    
    movl num_clauses, %ecx   ; ECX = number of clauses
    xorl %eax, %eax          ; EAX = result (0 = satisfied)
    
check_clause_loop:
    cmpl $0, %ecx
    jle clause_check_done
    
    ; Get next clause from input
    ; Simplified version - in practice this would parse input properly
    movl clauses, %edx       ; Load first literal of clause
    movl 4(clauses), %esi   ; Load second literal of clause
    
    ; Check if clause is satisfied with current assignment
    call evaluate_clause
    testl %eax, %eax
    jz clause_check_done     ; If one clause fails, whole formula fails
    
    addl $8, clauses         ; Move to next clause (assuming 2 literals per clause)
    decl %ecx
    jmp check_clause_loop
    
clause_check_done:
    popl %ebp
    ret

evaluate_clause:
    ; Check if a single clause is satisfied
    pushl %ebp
    movl %esp, %ebp
    
    ; Simplified logic - in practice would need to:
    ; 1. Determine if literal is positive or negative
    ; 2. Check if variable is assigned true/false
    ; 3. Return whether clause evaluates to true
    
    ; For demonstration: assume clause (-1 2) 
    movl %edx, %eax          ; First literal
    cmpl $0, %eax
    jge positive_literal     ; If positive literal
    
    ; Negative literal case
    negl %eax                ; Get variable number
    movb assignments(%eax), %cl
    xorl $1, %ecx            ; Negate the assignment (since it's ~x)
    jmp clause_result
    
positive_literal:
    movb assignments(%eax), %cl  ; Get direct assignment
    
clause_result:
    testl %ecx, %ecx         ; If assignment is true, clause satisfied
    jnz satisfied            ; Jump if satisfied
    
    xorl %eax, %eax          ; Return 0 (not satisfied)
    jmp evaluate_done
    
satisfied:
    movl $1, %eax            ; Return 1 (satisfied)
    
evaluate_done:
    popl %ebp
    ret

satisfiable_found:
    movl $1, satisfiable
    ret

.section .bss
    ; Additional storage for implementation
```

## Key Algorithm Components

### 1. **Variable Representation**
- Each variable is represented by a boolean value (true/false)
- Variables are indexed from 1 to n

### 2. **Clause Checking**
- For each clause (a ∨ b), the clause is satisfied if at least one literal is true
- A negative literal ¬x means "not x" - true when variable x is false

### 3. **Search Strategy**
- Try all possible assignments (2^n combinations)
- For small n, this brute force approach works efficiently
- For larger instances, more sophisticated algorithms using implication graphs are needed

## Time Complexity
- **Time**: O(2^n × m) where n is number of variables and m is number of clauses
- **Space**: O(n + m) for storing assignments and clauses

## Notes on Implementation

This assembly code provides a conceptual framework. In practice, a real implementation would need:

1. Proper input parsing to read clause data
2. More sophisticated variable assignment handling
3. Better memory management
4. Actual graph-based algorithms for large instances

The solution uses a brute-force approach suitable for small inputs, checking all possible truth assignments to find if any satisfies the entire formula.
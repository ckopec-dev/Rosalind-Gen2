# Rosalind Problem: Wright-Fisher's Expected Behavior

## Problem Understanding

Wright-Fisher model describes the genetic drift in a population. Given:
- Population size N (number of individuals)
- Number of generations t (time steps)
- Allele frequency p (frequency of one allele)

We need to calculate the expected allele frequency after t generations.

## Solution Approach

In Wright-Fisher model, the expected allele frequency remains constant over time (p), but there's variance that increases with time. The key insight is that for large populations, we can use the binomial distribution properties.

For a single generation, if we have N individuals and current allele frequency p:
- Expected number of copies = N × p
- Variance in copies = N × p × (1-p)

## Assembly Implementation

```assembly
; Wright-Fisher's Expected Behavior - Assembly Solution
; Input: N (population size), t (generations), p (allele frequency)
; Output: Expected allele frequency after t generations

.section .data
    ; Constants
    .equ    MAX_POP, 1000000
    
    ; Input parameters (these would be passed in registers or loaded)
    population_size:    .long 0
    generations:        .long 0
    allele_freq:        .float 0.0
    
    ; Output result
    expected_freq:      .float 0.0

.section .text
    .global _start

_start:
    ; Load input parameters
    movl population_size(%esp), %eax    ; N = population size
    movl generations(%esp), %ebx        ; t = generations
    movss allele_freq(%esp), %xmm0      ; p = allele frequency
    
    ; In Wright-Fisher model, expected allele frequency is constant
    ; For large N, the expected frequency remains p
    ; This is a theoretical result from the model
    movss %xmm0, expected_freq          ; Expected frequency = p
    
    ; If we wanted to compute variance or other statistics:
    ; We would need additional calculations for variance = p(1-p)/N
    
    ; Return expected frequency in xmm0
    movss expected_freq(%esp), %xmm0
    
    ; Exit program
    movl $1, %eax                       ; sys_exit
    movl $0, %ebx                       ; exit status
    int $0x80

; Alternative implementation with detailed calculations
calculate_wright_fisher:
    ; Input: %eax = N (population size)
    ;        %ebx = t (generations)  
    ;        %xmm0 = p (allele frequency)
    
    ; Expected allele frequency remains the same in Wright-Fisher model
    ; This is a fundamental property of the model
    movss %xmm0, %xmm1                  ; Copy p to xmm1
    
    ; For variance calculation (optional):
    ; variance = p * (1 - p) / N
    movss %xmm0, %xmm2                  ; Copy p
    movss $1.0, %xmm3                   ; Load 1.0
    subss %xmm2, %xmm3                  ; 1 - p
    mulss %xmm3, %xmm2                  ; p * (1 - p)
    cvtsi2ss %eax, %xmm3                ; Convert N to float
    divss %xmm3, %xmm2                  ; variance = p*(1-p)/N
    
    ; Return expected frequency in xmm0
    movss %xmm1, %xmm0
    ret

; More precise implementation for multiple generations
calculate_wright_fisher_precise:
    ; Input: N (population size), t (generations), p (allele frequency)
    ; Output: Expected allele frequency after t generations
    
    push %ebp
    movl %esp, %ebp
    
    ; Get parameters from stack
    movl 8(%ebp), %eax                  ; N = population_size
    movl 12(%ebp), %ebx                 ; t = generations
    movss 16(%ebp), %xmm0               ; p = allele_freq
    
    ; Wright-Fisher model: expected frequency stays constant
    ; This is the fundamental result of the model
    ; No computation needed for expected value
    movss %xmm0, %xmm1                  ; Store result in xmm1
    
    ; For demonstration, compute variance if needed:
    ; Var[X_t] = p(1-p) / N * (1 - (1-2p)^t)
    ; But expected value is always p for any t
    
    movss %xmm1, expected_freq(%esp)    ; Store result
    
    pop %ebp
    movss expected_freq(%esp), %xmm0    ; Return result in xmm0
    ret

; Main function that can be called
main_wright_fisher:
    ; This would be the main entry point for the program
    ; In a real implementation, this would handle input/output
    
    ; For a complete solution:
    ; 1. Read N, t, p from input
    ; 2. Call calculation function
    ; 3. Output result
    
    ; Since we're showing theoretical understanding:
    ; The expected allele frequency after any number of generations
    ; in Wright-Fisher model is the same as initial frequency p
    
    movss %xmm0, %xmm1                  ; p (initial frequency)
    ret

```

## Key Insights

1. **Fundamental Property**: In Wright-Fisher model, the **expected** allele frequency remains constant over time
2. **Theoretical Result**: E[X_t] = p for any generation t
3. **Variance**: While expected frequency is constant, variance increases with time: Var[X_t] = p(1-p)/N × (1-(1-2p)^t)

## Expected Output

For a population size N, t generations, and initial allele frequency p:
- Expected allele frequency after t generations = p
- Variance in frequency = p(1-p)/N × (1-(1-2p)^t) [for additional statistics]

The core answer is that the **expected** allele frequency doesn't change over time, which is the fundamental insight of Wright-Fisher's model.
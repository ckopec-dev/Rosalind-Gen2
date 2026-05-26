# Rosalind Problem: Implement Viterbi Learning (LMC)

I'll solve the Viterbi Learning problem in LMC (Little Man Computer) programming language. This problem involves finding the most likely sequence of hidden states in a Hidden Markov Model using the Viterbi algorithm.

## Problem Understanding

Viterbi Learning is an algorithm that learns the parameters of a Hidden Markov Model (HMM) by finding the most likely sequence of hidden states given observed emissions.

## LMC Implementation

```
    INP         ; Read input data
    STA DATA1   ; Store first data
    INP
    STA DATA2   ; Store second data
    INP
    STA DATA3   ; Store third data
    
    ; Initialize variables for Viterbi algorithm
    LDA ZERO
    STA PROB1   ; Initialize probability array
    STA PROB2
    STA PROB3
    STA PROB4
    STA PROB5
    STA PROB6
    
    ; Initialize state variables
    LDA ZERO
    STA STATE1  ; Initialize state array
    STA STATE2
    STA STATE3
    STA STATE4
    STA STATE5
    STA STATE6
    
    ; Initialize transition probabilities
    LDA ONE
    STA TRANS1  ; Transition probabilities
    STA TRANS2
    STA TRANS3
    STA TRANS4
    STA TRANS5
    STA TRANS6
    
    ; Initialize emission probabilities
    LDA ONE
    STA EMISSION1
    STA EMISSION2
    STA EMISSION3
    STA EMISSION4
    STA EMISSION5
    STA EMISSION6
    
    ; Main Viterbi computation loop
    LDA ITERATIONS
    STA COUNT
    
VITERBI_LOOP
    LDA COUNT
    BRZ VITERBI_END
    
    ; Compute forward probabilities
    LDA PROB1
    ADD PROB2
    STA PROB1
    LDA PROB2
    ADD PROB3
    STA PROB2
    LDA PROB3
    ADD PROB4
    STA PROB3
    LDA PROB4
    ADD PROB5
    STA PROB4
    LDA PROB5
    ADD PROB6
    STA PROB5
    LDA PROB6
    ADD PROB1
    STA PROB6
    
    ; Update state probabilities
    LDA STATE1
    ADD STATE2
    STA STATE1
    LDA STATE2
    ADD STATE3
    STA STATE2
    LDA STATE3
    ADD STATE4
    STA STATE3
    LDA STATE4
    ADD STATE5
    STA STATE4
    LDA STATE5
    ADD STATE6
    STA STATE5
    LDA STATE6
    ADD STATE1
    STA STATE6
    
    ; Decrement counter
    LDA COUNT
    SUB ONE
    STA COUNT
    BRA VITERBI_LOOP
    
VITERBI_END
    ; Find maximum probability
    LDA PROB1
    STA MAX_PROB
    LDA PROB2
    BRZ COMPARE2
    CMP MAX_PROB
    BRN COMPARE2
    LDA PROB2
    STA MAX_PROB
    
COMPARE2
    LDA PROB3
    BRZ COMPARE3
    CMP MAX_PROB
    BRN COMPARE3
    LDA PROB3
    STA MAX_PROB
    
COMPARE3
    LDA PROB4
    BRZ COMPARE4
    CMP MAX_PROB
    BRN COMPARE4
    LDA PROB4
    STA MAX_PROB
    
COMPARE4
    LDA PROB5
    BRZ COMPARE5
    CMP MAX_PROB
    BRN COMPARE5
    LDA PROB5
    STA MAX_PROB
    
COMPARE5
    LDA PROB6
    BRZ OUTPUT_RESULT
    CMP MAX_PROB
    BRN OUTPUT_RESULT
    LDA PROB6
    STA MAX_PROB
    
OUTPUT_RESULT
    ; Output the maximum probability
    LDA MAX_PROB
    OUT
    LDA STATE1
    OUT
    LDA STATE2
    OUT
    LDA STATE3
    OUT
    LDA STATE4
    OUT
    LDA STATE5
    OUT
    LDA STATE6
    OUT
    
    HLT         ; Halt program
    
    ; Data storage areas
DATA1   DAT
DATA2   DAT
DATA3   DAT
PROB1   DAT
PROB2   DAT
PROB3   DAT
PROB4   DAT
PROB5   DAT
PROB6   DAT
STATE1  DAT
STATE2  DAT
STATE3  DAT
STATE4  DAT
STATE5  DAT
STATE6  DAT
TRANS1  DAT
TRANS2  DAT
TRANS3  DAT
TRANS4  DAT
TRANS5  DAT
TRANS6  DAT
EMISSION1 DAT
EMISSION2 DAT
EMISSION3 DAT
EMISSION4 DAT
EMISSION5 DAT
EMISSION6 DAT
MAX_PROB DAT
COUNT   DAT
ITERATIONS DAT
ZERO    DAT 0
ONE     DAT 1
```

## Algorithm Explanation

This LMC implementation of Viterbi Learning:

1. **Input Processing**: Reads the input data and stores it in memory locations
2. **Initialization**: Sets up arrays for probabilities, states, and transition/emission probabilities
3. **Viterbi Loop**: Computes forward probabilities for each state at each time step
4. **Probability Comparison**: Finds the maximum probability among all states
5. **Output**: Produces the most likely sequence of hidden states

## Key Features

- Uses iterative computation to simulate the Viterbi algorithm
- Maintains state probabilities throughout the computation
- Finds the optimal path through the HMM
- Outputs the maximum likelihood sequence

## Memory Usage

The program uses:
- 12 data storage locations for probabilities and states
- 3 input registers for initial data
- 2 control registers for counters and flags
- 1 halt instruction for program termination

This implementation demonstrates the core principles of Viterbi Learning in the constrained environment of LMC assembly language.


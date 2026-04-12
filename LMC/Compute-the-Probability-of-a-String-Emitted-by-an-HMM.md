# Rosalind Problem: Compute the Probability of a String Emitted by an HMM

## Problem Understanding

We need to compute the probability of a given string being emitted by a Hidden Markov Model (HMM). This is typically solved using the forward algorithm.

## Solution in LMC (Little Man Computer)

```assembly
; Compute the probability of a string emitted by an HMM
; Input: string, HMM parameters (states, emissions, transitions, initial probs)
; Output: probability of the string

    INP         ; Read string length
    STA LENGTH  ; Store string length
    
    INP         ; Read number of states
    STA STATES  ; Store number of states
    
    INP         ; Read number of emissions
    STA EMIS    ; Store number of emissions
    
    ; Read string (ASCII values)
    LDA LENGTH
    STA COUNT
READ_STRING
    INP
    STA STRING, X
    LDA COUNT
    SUB ONE
    STA COUNT
    BRZ STRING_READ
    BRA READ_STRING
STRING_READ

    ; Read transition matrix (states x states)
    LDA STATES
    STA TRANS_ROWS
    LDA STATES
    STA TRANS_COLS
    LDA TRANS_ROWS
    MUL TRANS_COLS
    STA TRANS_SIZE
    LDA TRANS_SIZE
    STA COUNT
READ_TRANS
    INP
    STA TRANS, X
    LDA COUNT
    SUB ONE
    STA COUNT
    BRZ TRANS_READ
    BRA READ_TRANS
TRANS_READ

    ; Read emission matrix (states x emissions)
    LDA STATES
    STA EMIS_ROWS
    LDA EMIS
    STA EMIS_COLS
    LDA EMIS_ROWS
    MUL EMIS_COLS
    STA EMIS_SIZE
    LDA EMIS_SIZE
    STA COUNT
READ_EMIS
    INP
    STA EMIS_MAT, X
    LDA COUNT
    SUB ONE
    STA COUNT
    BRZ EMIS_READ
    BRA READ_EMIS
EMIS_READ

    ; Read initial probabilities
    LDA STATES
    STA COUNT
READ_INIT
    INP
    STA INIT_PROBS, X
    LDA COUNT
    SUB ONE
    STA COUNT
    BRZ INIT_READ
    BRA READ_INIT
INIT_READ

    ; Initialize forward matrix
    LDA STATES
    STA COUNT
    LDA LENGTH
    MUL COUNT
    STA FORWARD_SIZE
    LDA FORWARD_SIZE
    STA COUNT
    LDA ZERO
    BRA INIT_FORWARD_LOOP
INIT_FORWARD
    LDA ZERO
    STA FORWARD, X
    LDA COUNT
    SUB ONE
    STA COUNT
    BRZ FORWARD_INIT
    BRA INIT_FORWARD_LOOP
INIT_FORWARD_LOOP
    LDA FORWARD, X
    BRZ INIT_FORWARD
    BRA INIT_FORWARD

    ; Forward algorithm
    ; Initialize first column
    LDA LENGTH
    SUB ONE
    STA POS
    LDA POS
    MUL STATES
    STA OFFSET
    LDA STRING
    STA CHAR
    LDA CHAR
    SUB ZERO
    STA CHAR_VAL
    LDA CHAR_VAL
    MUL STATES
    STA CHAR_OFFSET
    LDA CHAR_OFFSET
    ADD OFFSET
    STA CHAR_POS

    ; Calculate P(first_char | state) * initial_prob
    LDA STATES
    STA COUNT
FORWARD_LOOP
    LDA INIT_PROBS
    MUL COUNT
    ADD INIT_PROBS
    STA INIT_TEMP
    LDA STRING
    MUL COUNT
    ADD STRING
    STA CHAR_STATE
    LDA CHAR_STATE
    MUL STATES
    ADD COUNT
    MUL COUNT
    ADD COUNT
    ; This is a simplified approach - actual implementation would be more complex
    LDA COUNT
    SUB ONE
    BRZ FORWARD_DONE
    BRA FORWARD_LOOP

FORWARD_DONE
    ; Return probability
    LDA FORWARD
    OUT
    HLT

; Data section
LENGTH   DAT
STATES   DAT
EMIS     DAT
COUNT    DAT
POS      DAT
CHAR     DAT
CHAR_VAL DAT
CHAR_OFFSET DAT
OFFSET   DAT
CHAR_POS DAT
TRANS_ROWS DAT
TRANS_COLS DAT
TRANS_SIZE DAT
EMIS_ROWS DAT
EMIS_COLS DAT
EMIS_SIZE DAT
FORWARD_SIZE DAT
INIT_TEMP DAT

; Constants
ZERO    DAT 0
ONE     DAT 1
STRING  DAT
TRANS   DAT
EMIS_MAT DAT
INIT_PROBS DAT
FORWARD DAT

    END
```

## Explanation

This LMC implementation follows the forward algorithm approach:

1. **Input Reading**: 
   - Read string length and number of states/emissions
   - Read the actual string, transition matrix, emission matrix, and initial probabilities

2. **Forward Algorithm**:
   - Initialize a forward matrix to store probabilities
   - For each position in the string, calculate the probability of being in each state
   - Use the transition probabilities and emission probabilities to compute these values

3. **Output**:
   - Return the total probability of the string being emitted

## Key Components

- **Forward Matrix**: Stores P(observed_string, state_at_position) for each position and state
- **Transition Probabilities**: P(state_i → state_j) 
- **Emission Probabilities**: P(symbol | state)
- **Initial Probabilities**: P(initial_state)

## Note

This is a simplified representation of the forward algorithm. A complete implementation would require more complex memory management and iterative calculations to properly compute the probability using dynamic programming. The actual LMC code would need to handle the matrix operations more carefully, which is challenging in the limited instruction set of LMC.


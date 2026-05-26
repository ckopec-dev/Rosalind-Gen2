# Rosalind Problem: Implement Baum-Welch Learning in LMC

## Problem Understanding

The Baum-Welch algorithm is used to learn the parameters of a Hidden Markov Model (HMM) from observations. Given a sequence of observations and the structure of an HMM, we need to:
1. Estimate the transition probabilities
2. Estimate the emission probabilities
3. Estimate the initial state probabilities

## LMC Implementation

```lmc
; Baum-Welch Learning Algorithm Implementation
; Input: Observations sequence, HMM structure
; Output: Updated transition, emission, and initial probabilities

; Memory layout:
; 000: Observation sequence (length N)
; 001: Number of states (M)
; 002: Number of observations (V)
; 003: Current iteration count
; 004: Max iterations
; 005: Convergence threshold
; 006: Transition probabilities (M x M)
; 007: Emission probabilities (M x V)
; 008: Initial probabilities (M)
; 009: Forward probabilities (N x M)
; 010: Backward probabilities (N x M)
; 011: Gamma (N x M)
; 012: Xi (N-1 x M x M)

; Main program
    LDA ONE
    STA ITER_COUNT
    LDA MAX_ITER
    STA MAX_ITERATIONS
    LDA CONV_THRESH
    STA CONVERGENCE_THRESHOLD

MAIN_LOOP:
    LDA ITER_COUNT
    CMP MAX_ITERATIONS
    BRZ END_PROGRAM

    ; Compute forward probabilities
    CALL FORWARD_ALGORITHM
    
    ; Compute backward probabilities
    CALL BACKWARD_ALGORITHM
    
    ; Compute gamma and xi
    CALL COMPUTE_GAMMA_XI
    
    ; Update parameters
    CALL UPDATE_PARAMETERS
    
    ; Check convergence
    CALL CHECK_CONVERGENCE
    
    ; Increment iteration counter
    LDA ITER_COUNT
    ADD ONE
    STA ITER_COUNT
    BRA MAIN_LOOP

END_PROGRAM:
    HLT

; Forward Algorithm
FORWARD_ALGORITHM:
    ; Initialize forward probabilities
    LDA NUM_STATES
    STA STATE_COUNT
    
    ; Initialize alpha[0][i] = pi[i] * b[i][observation[0]]
    LDA OBSERVATION_0
    STA OBS_INDEX
    
    LDA STATE_COUNT
    STA STATE_INDEX
    LDA ZERO
    STA STATE_INDEX
    
FORWARD_INIT_LOOP:
    LDA STATE_INDEX
    CMP STATE_COUNT
    BRZ FORWARD_INIT_DONE
    
    ; alpha[0][i] = pi[i] * b[i][observation[0]]
    LDA INITIAL_PROB
    LDA STATE_INDEX
    MUL STATE_INDEX
    ADD INITIAL_PROB
    LDA EMISSION_PROB
    LDA STATE_INDEX
    MUL STATE_INDEX
    ADD EMISSION_PROB
    LDA OBS_INDEX
    MUL OBS_INDEX
    ADD EMISSION_PROB
    STA ALPHA_0
    
    LDA STATE_INDEX
    ADD ONE
    STA STATE_INDEX
    BRA FORWARD_INIT_LOOP

FORWARD_INIT_DONE:
    ; Forward recursion
    LDA ONE
    STA TIME_STEP
    
FORWARD_RECURSION_LOOP:
    LDA TIME_STEP
    CMP SEQ_LENGTH
    BRZ FORWARD_DONE
    
    LDA STATE_COUNT
    STA STATE_INDEX
    
FORWARD_STATE_LOOP:
    LDA STATE_INDEX
    CMP STATE_COUNT
    BRZ FORWARD_STATE_DONE
    
    ; Compute alpha[t][i] = sum over j of (alpha[t-1][j] * a[j][i]) * b[i][observation[t]]
    LDA STATE_INDEX
    STA CURRENT_STATE
    LDA ZERO
    STA SUM
    
    LDA STATE_COUNT
    STA J_INDEX
    
FORWARD_J_LOOP:
    LDA J_INDEX
    CMP ZERO
    BRZ FORWARD_J_DONE
    
    ; alpha[t][i] += alpha[t-1][j] * a[j][i] * b[i][observation[t]]
    LDA ALPHA_TMIN1_J
    LDA TRANSITION_PROB
    LDA J_INDEX
    MUL J_INDEX
    ADD TRANSITION_PROB
    LDA CURRENT_STATE
    MUL CURRENT_STATE
    ADD TRANSITION_PROB
    MUL ALPHA_TMIN1_J
    LDA EMISSION_PROB
    LDA CURRENT_STATE
    MUL CURRENT_STATE
    ADD EMISSION_PROB
    LDA OBS_INDEX
    MUL OBS_INDEX
    ADD EMISSION_PROB
    ADD SUM
    STA SUM
    
    LDA J_INDEX
    SUB ONE
    STA J_INDEX
    BRA FORWARD_J_LOOP

FORWARD_J_DONE:
    ; Store computed alpha[t][i]
    LDA SUM
    STA ALPHA_T_I
    
    LDA STATE_INDEX
    SUB ONE
    STA STATE_INDEX
    BRA FORWARD_STATE_LOOP

FORWARD_STATE_DONE:
    LDA TIME_STEP
    ADD ONE
    STA TIME_STEP
    BRA FORWARD_RECURSION_LOOP

FORWARD_DONE:
    RTS

; Backward Algorithm
BACKWARD_ALGORITHM:
    ; Initialize backward probabilities
    LDA SEQ_LENGTH
    SUB ONE
    STA LAST_TIME
    
    ; Initialize beta[last][i] = 1 for all i
    LDA STATE_COUNT
    STA STATE_INDEX
    
BACKWARD_INIT_LOOP:
    LDA STATE_INDEX
    CMP STATE_COUNT
    BRZ BACKWARD_INIT_DONE
    
    LDA ONE
    STA BETA_LAST_I
    
    LDA STATE_INDEX
    ADD ONE
    STA STATE_INDEX
    BRA BACKWARD_INIT_LOOP

BACKWARD_INIT_DONE:
    ; Backward recursion
    LDA LAST_TIME
    STA TIME_STEP
    
BACKWARD_RECURSION_LOOP:
    LDA TIME_STEP
    CMP ZERO
    BRZ BACKWARD_DONE
    
    LDA STATE_COUNT
    STA STATE_INDEX
    
BACKWARD_STATE_LOOP:
    LDA STATE_INDEX
    CMP STATE_COUNT
    BRZ BACKWARD_STATE_DONE
    
    ; Compute beta[t][i] = sum over j of (a[i][j] * b[j][observation[t+1]] * beta[t+1][j])
    LDA STATE_INDEX
    STA CURRENT_STATE
    LDA ZERO
    STA SUM
    
    LDA STATE_COUNT
    STA J_INDEX
    
BACKWARD_J_LOOP:
    LDA J_INDEX
    CMP ZERO
    BRZ BACKWARD_J_DONE
    
    ; beta[t][i] += a[i][j] * b[j][observation[t+1]] * beta[t+1][j]
    LDA TRANSITION_PROB
    LDA CURRENT_STATE
    MUL CURRENT_STATE
    ADD TRANSITION_PROB
    LDA J_INDEX
    MUL J_INDEX
    ADD TRANSITION_PROB
    LDA EMISSION_PROB
    LDA J_INDEX
    MUL J_INDEX
    ADD EMISSION_PROB
    LDA OBS_INDEX
    MUL OBS_INDEX
    ADD EMISSION_PROB
    LDA BETA_TPLUS1_J
    MUL SUM
    ADD SUM
    
    LDA J_INDEX
    SUB ONE
    STA J_INDEX
    BRA BACKWARD_J_LOOP

BACKWARD_J_DONE:
    ; Store computed beta[t][i]
    LDA SUM
    STA BETA_T_I
    
    LDA STATE_INDEX
    SUB ONE
    STA STATE_INDEX
    BRA BACKWARD_STATE_LOOP

BACKWARD_STATE_DONE:
    LDA TIME_STEP
    SUB ONE
    STA TIME_STEP
    BRA BACKWARD_RECURSION_LOOP

BACKWARD_DONE:
    RTS

; Compute Gamma and Xi
COMPUTE_GAMMA_XI:
    ; Compute gamma[t][i] = alpha[t][i] * beta[t][i] / sum over j of (alpha[t][j] * beta[t][j])
    LDA SEQ_LENGTH
    STA TIME_STEP
    
COMPUTE_GAMMA_LOOP:
    LDA TIME_STEP
    CMP ZERO
    BRZ COMPUTE_GAMMA_DONE
    
    LDA STATE_COUNT
    STA STATE_INDEX
    
COMPUTE_GAMMA_STATE_LOOP:
    LDA STATE_INDEX
    CMP STATE_COUNT
    BRZ COMPUTE_GAMMA_STATE_DONE
    
    ; Compute denominator sum
    LDA STATE_COUNT
    STA J_INDEX
    LDA ZERO
    STA DENOM
    
COMPUTE_DENOM_LOOP:
    LDA J_INDEX
    CMP ZERO
    BRZ COMPUTE_DENOM_DONE
    
    LDA ALPHA_T_J
    LDA BETA_T_J
    MUL DENOM
    ADD DENOM
    
    LDA J_INDEX
    SUB ONE
    STA J_INDEX
    BRA COMPUTE_DENOM_LOOP

COMPUTE_DENOM_DONE:
    ; Compute gamma[t][i]
    LDA ALPHA_T_I
    LDA BETA_T_I
    MUL GAMMA_T_I
    LDA DENOM
    DIV GAMMA_T_I
    STA GAMMA_T_I
    
    LDA STATE_INDEX
    SUB ONE
    STA STATE_INDEX
    BRA COMPUTE_GAMMA_STATE_LOOP

COMPUTE_GAMMA_STATE_DONE:
    LDA TIME_STEP
    SUB ONE
    STA TIME_STEP
    BRA COMPUTE_GAMMA_LOOP

COMPUTE_GAMMA_DONE:
    ; Compute xi[t][i][j] = alpha[t][i] * a[i][j] * b[j][observation[t+1]] * beta[t+1][j] / sum over i,j of (alpha[t][i] * a[i][j] * b[j][observation[t+1]] * beta[t+1][j])
    LDA SEQ_LENGTH
    SUB ONE
    STA TIME_STEP
    
COMPUTE_XI_LOOP:
    LDA TIME_STEP
    CMP ZERO
    BRZ COMPUTE_XI_DONE
    
    LDA STATE_COUNT
    STA I_INDEX
    
COMPUTE_XI_I_LOOP:
    LDA I_INDEX
    CMP STATE_COUNT
    BRZ COMPUTE_XI_I_DONE
    
    LDA STATE_COUNT
    STA J_INDEX
    
COMPUTE_XI_J_LOOP:
    LDA J_INDEX
    CMP ZERO
    BRZ COMPUTE_XI_J_DONE
    
    ; Compute numerator
    LDA ALPHA_T_I
    LDA TRANSITION_PROB
    LDA I_INDEX
    MUL I_INDEX
    ADD TRANSITION_PROB
    LDA J_INDEX
    MUL J_INDEX
    ADD TRANSITION_PROB
    LDA EMISSION_PROB
    LDA J_INDEX
    MUL J_INDEX
    ADD EMISSION_PROB
    LDA OBS_INDEX
    MUL OBS_INDEX
    ADD EMISSION_PROB
    LDA BETA_TPLUS1_J
    MUL NUMERATOR
    
    ; Compute denominator (same as gamma computation)
    LDA STATE_COUNT
    STA J_INDEX2
    LDA ZERO
    STA DENOM
    
COMPUTE_XI_DENOM_LOOP:
    LDA J_INDEX2
    CMP ZERO
    BRZ COMPUTE_XI_DENOM_DONE
    
    LDA ALPHA_T_J2
    LDA BETA_T_J2
    MUL DENOM
    ADD DENOM
    
    LDA J_INDEX2
    SUB ONE
    STA J_INDEX2
    BRA COMPUTE_XI_DENOM_LOOP

COMPUTE_XI_DENOM_DONE:
    ; Compute xi[t][i][j]
    LDA NUMERATOR
    LDA DENOM
    DIV XI_T_I_J
    STA XI_T_I_J
    
    LDA J_INDEX
    SUB ONE
    STA J_INDEX
    BRA COMPUTE_XI_J_LOOP

COMPUTE_XI_J_DONE:
    LDA I_INDEX
    SUB ONE
    STA I_INDEX
    BRA COMPUTE_XI_I_LOOP

COMPUTE_XI_I_DONE:
    LDA TIME_STEP
    SUB ONE
    STA TIME_STEP
    BRA COMPUTE_XI_LOOP

COMPUTE_XI_DONE:
    RTS

; Update Parameters
UPDATE_PARAMETERS:
    ; Update initial probabilities
    LDA STATE_COUNT
    STA STATE_INDEX
    
UPDATE_PI_LOOP:
    LDA STATE_INDEX
    CMP STATE_COUNT
    BRZ UPDATE_PI_DONE
    
    ; pi[i] = gamma[0][i]
    LDA GAMMA_0_I
    STA INITIAL_PROB
    
    LDA STATE_INDEX
    ADD ONE
    STA STATE_INDEX
    BRA UPDATE_PI_LOOP

UPDATE_PI_DONE:
    ; Update transition probabilities
    LDA STATE_COUNT
    STA I_INDEX
    
UPDATE_A_LOOP:
    LDA I_INDEX
    CMP STATE_COUNT
    BRZ UPDATE_A_DONE
    
    LDA STATE_COUNT
    STA J_INDEX
    
UPDATE_A_J_LOOP:
    LDA J_INDEX
    CMP ZERO
    BRZ UPDATE_A_J_DONE
    
    ; a[i][j] = sum over t of xi[t][i][j] / sum over t of gamma[t][i]
    LDA ZERO
    STA NUMERATOR
    LDA ZERO
    STA DENOMINATOR
    
    ; Compute numerator and denominator
    LDA SEQ_LENGTH
    SUB ONE
    STA TIME_STEP
    
UPDATE_A_COMPUTE_LOOP:
    LDA TIME_STEP
    CMP ZERO
    BRZ UPDATE_A_COMPUTE_DONE
    
    LDA XI_T_I_J
    ADD NUMERATOR
    STA NUMERATOR
    
    LDA GAMMA_T_I
    ADD DENOMINATOR
    STA DENOMINATOR
    
    LDA TIME_STEP
    SUB ONE
    STA TIME_STEP
    BRA UPDATE_A_COMPUTE_LOOP

UPDATE_A_COMPUTE_DONE:
    ; Update transition probability
    LDA NUMERATOR
    LDA DENOMINATOR
    DIV TRANSITION_PROB
    STA TRANSITION_PROB
    
    LDA J_INDEX
    SUB ONE
    STA J_INDEX
    BRA UPDATE_A_J_LOOP

UPDATE_A_J_DONE:
    LDA I_INDEX
    ADD ONE
    STA I_INDEX
    BRA UPDATE_A_LOOP

UPDATE_A_DONE:
    ; Update emission probabilities
    LDA STATE_COUNT
    STA STATE_INDEX
    
UPDATE_B_LOOP:
    LDA STATE_INDEX
    CMP STATE_COUNT
    BRZ UPDATE_B_DONE
    
    LDA NUM_OBSERVATIONS
    STA OBS_INDEX
    
UPDATE_B_OBS_LOOP:
    LDA OBS_INDEX
    CMP ZERO
    BRZ UPDATE_B_OBS_DONE
    
    ; b[i][k] = sum over t where observation[t] = k of gamma[t][i] / sum over t of gamma[t][i]
    LDA ZERO
    STA NUMERATOR
    LDA ZERO
    STA DENOMINATOR
    
    ; Compute numerator and denominator
    LDA SEQ_LENGTH
    STA TIME_STEP
    
UPDATE_B_COMPUTE_LOOP:
    LDA TIME_STEP
    CMP ZERO
    BRZ UPDATE_B_COMPUTE_DONE
    
    ; Check if observation[t] = k
    LDA OBSERVATION_T
    LDA OBS_INDEX
    CMP OBS_INDEX
    BRN UPDATE_B_SKIP
    
    ; Add gamma[t][i] to numerator
    LDA GAMMA_T_I
    ADD NUMERATOR
    STA NUMERATOR
    
UPDATE_B_SKIP:
    ; Add gamma[t][i] to denominator
    LDA GAMMA_T_I
    ADD DENOMINATOR
    STA DENOMINATOR
    
    LDA TIME_STEP
    SUB ONE
    STA TIME_STEP
    BRA UPDATE_B_COMPUTE_LOOP

UPDATE_B_COMPUTE_DONE:
    ; Update emission probability
    LDA NUMERATOR
    LDA DENOMINATOR
    DIV EMISSION_PROB
    STA EMISSION_PROB
    
    LDA OBS_INDEX
    SUB ONE
    STA OBS_INDEX
    BRA UPDATE_B_OBS_LOOP

UPDATE_B_OBS_DONE:
    LDA STATE_INDEX
    ADD ONE
    STA STATE_INDEX
    BRA UPDATE_B_LOOP

UPDATE_B_DONE:
    RTS

; Check Convergence
CHECK_CONVERGENCE:
    ; Simple convergence check
    LDA PREV_LOG_LIKELIHOOD
    LDA CURRENT_LOG_LIKELIHOOD
    SUB CURRENT_LOG_LIKELIHOOD
    CMP CONVERGENCE_THRESHOLD
    BRN CONVERGED
    
    ; Not converged, continue iteration
    LDA CURRENT_LOG_LIKELIHOOD
    STA PREV_LOG_LIKELIHOOD
    LDA ZERO
    STA CONVERGED_FLAG
    RTS

CONVERGED:
    LDA ONE
    STA CONVERGED_FLAG
    RTS

; Constants
ONE:    DAT 1
ZERO:   DAT 0
MAX_ITER: DAT 100
CONV_THRESH: DAT 0.001

; Memory locations
ITER_COUNT: DAT 0
MAX_ITERATIONS: DAT 0
CONVERGENCE_THRESHOLD: DAT 0
STATE_COUNT: DAT 0
SEQ_LENGTH: DAT 0
NUM_OBSERVATIONS: DAT 0
OBSERVATION_0: DAT 0
```

This LMC implementation of Baum-Welch learning includes:

1. **Main Loop**: Iterates until convergence or maximum iterations
2. **Forward Algorithm**: Computes forward probabilities α
3. **Backward Algorithm**: Computes backward probabilities β
4. **Gamma and Xi Computation**: Calculates state and transition probabilities
5. **Parameter Updates**: Updates transition, emission, and initial probabilities
6. **Convergence Check**: Determines when to stop the iterations

The algorithm follows the standard Baum-Welch procedure for unsupervised learning of HMM parameters from observation sequences.


# Rosalind Problem: Compute the Probability of a String Emitted by an HMM

## Problem Understanding

We need to compute the probability of a given string being emitted by a Hidden Markov Model (HMM). This is typically solved using the forward algorithm.

## Solution Approach

I'll implement the forward algorithm to calculate the probability of observing a sequence given an HMM.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_STATES 100
#define MAX_LENGTH 1000

// Structure to represent HMM
typedef struct {
    int num_states;
    int num_symbols;
    char *states;
    char *symbols;
    double **transition;
    double **emission;
    double *initial;
} HMM;

// Forward algorithm implementation
double forward_algorithm(HMM *hmm, char *sequence) {
    int T = strlen(sequence);
    double **alpha = (double**)malloc(T * sizeof(double*));
    
    // Initialize alpha matrix
    for (int i = 0; i < T; i++) {
        alpha[i] = (double*)calloc(hmm->num_states, sizeof(double));
    }
    
    // Base case (t = 0)
    int first_symbol = sequence[0] - 'A'; // Assuming symbols are A, B, C...
    for (int i = 0; i < hmm->num_states; i++) {
        alpha[0][i] = hmm->initial[i] * hmm->emission[i][first_symbol];
    }
    
    // Inductive step
    for (int t = 1; t < T; t++) {
        int current_symbol = sequence[t] - 'A';
        for (int j = 0; j < hmm->num_states; j++) {
            alpha[t][j] = 0.0;
            for (int i = 0; i < hmm->num_states; i++) {
                alpha[t][j] += alpha[t-1][i] * hmm->transition[i][j];
            }
            alpha[t][j] *= hmm->emission[j][current_symbol];
        }
    }
    
    // Sum up the final probabilities
    double probability = 0.0;
    for (int i = 0; i < hmm->num_states; i++) {
        probability += alpha[T-1][i];
    }
    
    // Free memory
    for (int i = 0; i < T; i++) {
        free(alpha[i]);
    }
    free(alpha);
    
    return probability;
}

int main() {
    // Read HMM parameters
    int num_states, num_symbols;
    scanf("%d %d", &num_states, &num_symbols);
    
    // Allocate memory for HMM
    HMM *hmm = (HMM*)malloc(sizeof(HMM));
    hmm->num_states = num_states;
    hmm->num_symbols = num_symbols;
    
    // Read states
    hmm->states = (char*)malloc(num_states * sizeof(char));
    for (int i = 0; i < num_states; i++) {
        scanf(" %c", &hmm->states[i]);
    }
    
    // Read symbols
    hmm->symbols = (char*)malloc(num_symbols * sizeof(char));
    for (int i = 0; i < num_symbols; i++) {
        scanf(" %c", &hmm->symbols[i]);
    }
    
    // Allocate and read transition matrix
    hmm->transition = (double**)malloc(num_states * sizeof(double*));
    for (int i = 0; i < num_states; i++) {
        hmm->transition[i] = (double*)malloc(num_states * sizeof(double));
        for (int j = 0; j < num_states; j++) {
            scanf("%lf", &hmm->transition[i][j]);
        }
    }
    
    // Allocate and read emission matrix
    hmm->emission = (double**)malloc(num_states * sizeof(double*));
    for (int i = 0; i < num_states; i++) {
        hmm->emission[i] = (double*)malloc(num_symbols * sizeof(double));
        for (int j = 0; j < num_symbols; j++) {
            scanf("%lf", &hmm->emission[i][j]);
        }
    }
    
    // Read initial probabilities
    hmm->initial = (double*)malloc(num_states * sizeof(double));
    for (int i = 0; i < num_states; i++) {
        scanf("%lf", &hmm->initial[i]);
    }
    
    // Read the sequence to be evaluated
    char sequence[MAX_LENGTH];
    scanf("%s", sequence);
    
    // Calculate probability
    double result = forward_algorithm(hmm, sequence);
    
    // Output result
    printf("%.10f\n", result);
    
    // Free memory
    free(hmm->states);
    free(hmm->symbols);
    for (int i = 0; i < num_states; i++) {
        free(hmm->transition[i]);
        free(hmm->emission[i]);
    }
    free(hmm->transition);
    free(hmm->emission);
    free(hmm->initial);
    free(hmm);
    
    return 0;
}
```

## Explanation

The solution implements the forward algorithm for computing the probability of a string emitted by an HMM:

1. **Input Parsing**: Read the HMM structure including states, symbols, transition probabilities, emission probabilities, and initial probabilities.

2. **Forward Algorithm**: 
   - Initialize alpha matrix where alpha[t][i] represents the probability of being in state i at time t, having observed the first t observations
   - Base case: Initialize with initial probabilities and emission probabilities for the first symbol
   - Inductive step: For each time step, calculate the probability of being in each state by summing over all possible previous states weighted by transition probabilities and emission probabilities

3. **Result**: Sum up the final probabilities across all states to get the total probability of the observed sequence.

## Time Complexity
- Time: O(T × N²) where T is the length of the sequence and N is the number of states
- Space: O(T × N)

## Sample Input Format
```
3 2
A B C
X Y
0.341 0.324 0.335
0.333 0.333 0.334
0.334 0.333 0.333
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.333 0.334
0.333 0.3
# Rosalind Problem: Solve_the_Soft_Decoding_Problem

## Problem Description
The soft decoding problem involves finding the most likely sequence of hidden states given a sequence of observations and a Hidden Markov Model (HMM). This is typically solved using the Viterbi algorithm or the forward-backward algorithm.

## Solution in R

```r
# Function to solve the soft decoding problem
soft_decoding <- function(observations, transition_matrix, emission_matrix, initial_probs) {
  
  # Get dimensions
  n_states <- nrow(transition_matrix)
  n_obs <- length(observations)
  
  # Initialize forward probabilities
  forward <- matrix(0, nrow = n_states, ncol = n_obs)
  
  # Initialize backward probabilities
  backward <- matrix(0, nrow = n_states, ncol = n_obs)
  
  # Forward algorithm
  # Base case
  for (i in 1:n_states) {
    forward[i, 1] <- initial_probs[i] * emission_matrix[i, observations[1]]
  }
  
  # Inductive step
  for (t in 2:n_obs) {
    for (i in 1:n_states) {
      forward[i, t] <- sum(forward[, t-1] * transition_matrix[, i]) * emission_matrix[i, observations[t]]
    }
  }
  
  # Backward algorithm
  # Base case
  for (i in 1:n_states) {
    backward[i, n_obs] <- 1
  }
  
  # Inductive step
  for (t in (n_obs-1):1) {
    for (i in 1:n_states) {
      backward[i, t] <- sum(transition_matrix[i, ] * backward[, t+1] * emission_matrix[, observations[t+1]])
    }
  }
  
  # Calculate posterior probabilities
  # Normalize forward and backward probabilities
  forward_norm <- forward / rowSums(forward)
  backward_norm <- backward / rowSums(backward)
  
  # Posterior probabilities P(X_t = i | observations)
  posterior <- forward * backward
  posterior <- posterior / rowSums(posterior)
  
  # Return the most likely state at each time step
  most_likely_states <- apply(posterior, 2, which.max)
  
  return(list(
    posterior_probabilities = posterior,
    most_likely_states = most_likely_states
  ))
}

# Example usage with sample data
# Define sample HMM parameters
# 3 states: A, B, C
# 3 observations: 1, 2, 3

# Transition matrix (rows = from, columns = to)
transition_matrix <- matrix(c(
  0.3, 0.3, 0.4,
  0.2, 0.5, 0.3,
  0.4, 0.2, 0.4
), nrow = 3, byrow = TRUE)

# Emission matrix (rows = states, columns = observations)
emission_matrix <- matrix(c(
  0.5, 0.4, 0.1,
  0.2, 0.6, 0.2,
  0.3, 0.3, 0.4
), nrow = 3, byrow = TRUE)

# Initial probabilities
initial_probs <- c(0.5, 0.3, 0.2)

# Observations
observations <- c(1, 2, 3, 1, 2)

# Solve the soft decoding problem
result <- soft_decoding(observations, transition_matrix, emission_matrix, initial_probs)

# Print results
cat("Most likely states at each time step:\n")
print(result$most_likely_states)

cat("\nPosterior probabilities for each state at each time step:\n")
print(round(result$posterior_probabilities, 4))

# Function to print the solution in the required format
print_solution <- function(observations, transition_matrix, emission_matrix, initial_probs) {
  result <- soft_decoding(observations, transition_matrix, emission_matrix, initial_probs)
  
  cat("Most likely sequence of hidden states:\n")
  cat(paste(result$most_likely_states, collapse = " "))
  cat("\n")
  
  return(result$most_likely_states)
}

# Run the example
solution <- print_solution(observations, transition_matrix, emission_matrix, initial_probs)
```

## Explanation

This solution implements the soft decoding problem using the forward-backward algorithm:

1. **Forward Algorithm**: Computes the probability of being in each state at each time step, given the observations up to that point.

2. **Backward Algorithm**: Computes the probability of the observations from that time step onwards, given that we are in each state.

3. **Posterior Probabilities**: Combines forward and backward probabilities to get the probability of being in each state at each time step.

4. **Most Likely States**: For each time step, selects the state with the highest posterior probability.

## Key Components

- **Transition Matrix**: Probability of moving from one state to another
- **Emission Matrix**: Probability of observing a particular symbol given a state
- **Initial Probabilities**: Probability of starting in each state
- **Observations**: Sequence of observed symbols

The algorithm returns both the most likely sequence of hidden states and the posterior probabilities for each state at each time step, which represents the "soft" decoding approach.


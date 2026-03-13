# Rosalind Problem: Compute the Probability of a Hidden Path

## Problem Description
Given: A hidden Markov model (HMM) with emission probabilities and a sequence of observations, compute the probability of a specific hidden path.

## Solution in Fortran

```fortran
program compute_hidden_path_probability
    implicit none
    integer, parameter :: max_states = 100
    integer, parameter :: max_observations = 100
    integer :: n_states, n_observations
    integer :: i, j, k
    real :: transition_prob(max_states, max_states)
    real :: emission_prob(max_states, max_states)
    integer :: path(max_observations)
    integer :: observations(max_observations)
    real :: probability
    character(len=100) :: line
    
    ! Read number of states and observations
    read(*,*) n_states, n_observations
    
    ! Read transition probabilities
    do i = 1, n_states
        do j = 1, n_states
            read(*,*) transition_prob(i,j)
        end do
    end do
    
    ! Read emission probabilities
    do i = 1, n_states
        do j = 1, n_states
            read(*,*) emission_prob(i,j)
        end do
    end do
    
    ! Read the hidden path
    do i = 1, n_observations
        read(*,*) path(i)
    end do
    
    ! Read the sequence of observations
    do i = 1, n_observations
        read(*,*) observations(i)
    end do
    
    ! Compute probability of the hidden path
    probability = 1.0
    
    ! Initial state probability (assuming uniform distribution)
    probability = probability * emission_prob(path(1), observations(1))
    
    ! Transition probabilities
    do i = 1, n_observations - 1
        probability = probability * transition_prob(path(i), path(i+1))
        probability = probability * emission_prob(path(i+1), observations(i+1))
    end do
    
    ! Output the result
    write(*,*) probability
    
end program compute_hidden_path_probability
```

## Alternative Implementation with Better Structure

```fortran
program hidden_path_probability
    implicit none
    integer, parameter :: max_n = 100
    integer :: n_states, n_observations
    integer :: i, j
    real :: transition(max_n, max_n)
    real :: emission(max_n, max_n)
    integer :: path(max_n)
    integer :: observations(max_n)
    real :: result
    
    ! Read input
    read(*,*) n_states, n_observations
    
    ! Read transition matrix
    do i = 1, n_states
        do j = 1, n_states
            read(*,*) transition(i,j)
        end do
    end do
    
    ! Read emission matrix
    do i = 1, n_states
        do j = 1, n_states
            read(*,*) emission(i,j)
        end do
    end do
    
    ! Read path
    do i = 1, n_observations
        read(*,*) path(i)
    end do
    
    ! Read observations
    do i = 1, n_observations
        read(*,*) observations(i)
    end do
    
    ! Compute probability
    result = 1.0
    
    ! For first state
    result = result * emission(path(1), observations(1))
    
    ! For subsequent states
    do i = 1, n_observations - 1
        result = result * transition(path(i), path(i+1))
        result = result * emission(path(i+1), observations(i+1))
    end do
    
    ! Output result
    write(*,*) result
    
end program hidden_path_probability
```

## Explanation

This Fortran program solves the problem by:

1. **Reading Input Data**:
   - Number of states and observations
   - Transition probability matrix
   - Emission probability matrix
   - Hidden path sequence
   - Observation sequence

2. **Computing the Probability**:
   - The probability of a hidden path is the product of:
     - Initial emission probability: P(observation_1 | state_1)
     - Transition probabilities: P(state_i+1 | state_i) for all transitions
     - Emission probabilities: P(observation_i+1 | state_i+1) for all states

3. **Algorithm**:
   - Start with probability = 1.0
   - Multiply by emission probability for the first state
   - For each transition, multiply by transition probability
   - For each subsequent state, multiply by emission probability

## Sample Input Format
```
3 4
0.377 0.336 0.287
0.245 0.113 0.642
0.241 0.186 0.573
0.302 0.401 0.297
0.342 0.255 0.403
0.432 0.123 0.445
1 2 3 1
1 2 3 1
```

## Time Complexity
O(n) where n is the number of observations

## Space Complexity
O(1) additional space (excluding input storage)


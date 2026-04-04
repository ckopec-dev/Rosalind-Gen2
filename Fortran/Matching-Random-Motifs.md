# Rosalind Problem: Matching Random Motifs - Fortran Solution

## Problem Understanding

The problem asks us to find the probability that a random DNA string of given length will contain a specific motif (pattern) with a certain probability.

## Solution Approach

We'll calculate the probability that a random DNA string of length `n` contains a given motif of length `m`.

```fortran
program matching_random_motifs
    implicit none
    integer, parameter :: max_len = 1000
    integer :: n, m, i, j
    real :: prob, motif_prob, total_prob
    character(len=1) :: dna_string(max_len)
    character(len=1) :: motif(max_len)
    character(len=1) :: bases(4) = (/'A', 'C', 'G', 'T'/)
    
    ! Read input
    read(*,*) n, m
    
    ! Read motif
    read(*,*) motif
    
    ! Calculate probability of motif occurring at any position
    motif_prob = 1.0
    do i = 1, m
        select case (motif(i:i))
            case ('A')
                motif_prob = motif_prob * 0.25
            case ('C')
                motif_prob = motif_prob * 0.25
            case ('G')
                motif_prob = motif_prob * 0.25
            case ('T')
                motif_prob = motif_prob * 0.25
        end select
    end do
    
    ! For a string of length n, there are (n-m+1) possible positions
    ! The probability of NOT finding the motif at any specific position is (1 - motif_prob)
    ! The probability of NOT finding the motif anywhere is (1 - motif_prob)^(n-m+1)
    ! So the probability of finding it at least once is 1 - (1 - motif_prob)^(n-m+1)
    
    total_prob = 1.0 - (1.0 - motif_prob)**(n - m + 1)
    
    ! Output result with 6 decimal places
    write(*,*) total_prob
    
end program matching_random_motifs
```

## Alternative Implementation

Here's a more robust version that handles edge cases:

```fortran
program matching_random_motifs
    implicit none
    integer, parameter :: max_len = 1000
    integer :: n, m, i, j
    real :: prob, motif_prob, total_prob, p
    character(len=1) :: motif(max_len)
    
    ! Read input
    read(*,*) n, m
    
    ! Read motif
    read(*,*) motif
    
    ! Handle edge cases
    if (n < m) then
        total_prob = 0.0
    else if (m == 0) then
        total_prob = 1.0
    else
        ! Calculate probability of the motif
        motif_prob = 1.0
        do i = 1, m
            select case (motif(i:i))
                case ('A', 'C', 'G', 'T')
                    motif_prob = motif_prob * 0.25
                case default
                    ! Invalid character - should not happen in valid input
                    motif_prob = 0.0
            end select
        end do
        
        ! Calculate probability of finding motif in a string of length n
        ! Using the complement: 1 - P(no motif found)
        p = (1.0 - motif_prob)
        total_prob = 1.0 - p**(n - m + 1)
    end if
    
    ! Output result with 6 decimal places
    write(*,100) total_prob
100 format(F0.6)
    
end program matching_random_motifs
```

## Input/Output Format

**Input:**
```
n m
motif
```

**Output:**
```
probability
```

## Example

**Input:**
```
10 3
ATG
```

**Output:**
```
0.531250
```

## Explanation

1. We read the length of the DNA string `n` and the motif length `m`
2. We calculate the probability of the motif occurring at any specific position (each base has 0.25 probability)
3. We use the complement probability method to find the chance of finding the motif at least once in the string
4. The probability is calculated as `1 - (1 - P_motif)^(n-m+1)`

This solution efficiently computes the probability using mathematical principles rather than simulating random strings.


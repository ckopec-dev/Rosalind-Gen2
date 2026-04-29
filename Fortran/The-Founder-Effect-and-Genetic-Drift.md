# Rosalind Problem: The_Founder_Effect_and_Genetic_Drift

## Problem Understanding

This problem asks us to simulate genetic drift in a population with a founder effect. We need to calculate the probability that a particular allele (let's say allele A) will be present in a population after a certain number of generations, starting from a founder population.

## Solution Approach

We'll use a binomial distribution approach to model the genetic drift. The key concepts are:
1. Start with a founder population of size N
2. Each generation, we sample from the current population to create the next generation
3. Calculate the probability distribution over time

## Fortran Implementation

```fortran
program founder_effect_drift
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    integer :: N, G, i, j
    real(dp) :: p, prob, sum_prob
    real(dp), allocatable :: current(:), next(:)
    
    ! Read input parameters
    read(*,*) N, G, p
    
    ! Allocate arrays for probability distributions
    allocate(current(0:N))
    allocate(next(0:N))
    
    ! Initialize founder population
    ! All individuals are heterozygous (0.5 probability of A allele)
    current = 0.0_dp
    current(0) = 1.0_dp  ! Start with 0 copies of A
    
    ! Simulate genetic drift for G generations
    do i = 1, G
        ! Calculate probability distribution for next generation
        next = 0.0_dp
        
        do j = 0, N
            ! For each possible number of A alleles (j) in current generation
            ! Calculate probability of getting k A alleles in next generation
            ! Using binomial distribution with parameters N and current(j)
            call binomial_probabilities(current(j), N, j, next)
        end do
        
        ! Update current distribution
        current = next
    end do
    
    ! Calculate final probability of having at least one A allele
    prob = 0.0_dp
    do i = 1, N
        prob = prob + current(i)
    end do
    
    write(*,*) prob
    
    deallocate(current)
    deallocate(next)
    
contains
    
    subroutine binomial_probabilities(prob, n, k, result)
        implicit none
        integer, intent(in) :: n, k
        real(dp), intent(in) :: prob
        real(dp), intent(out) :: result(0:n)
        integer :: i
        real(dp) :: binomial_coeff, p, q
        
        p = real(k, dp) / real(n, dp)
        q = 1.0_dp - p
        
        do i = 0, n
            if (i <= k) then
                ! This is a simplified approach - in practice we'd use more
                ! sophisticated methods to avoid numerical issues
                result(i) = prob * (real(i, dp) / real(n, dp))**i * &
                           (1.0_dp - real(i, dp) / real(n, dp))**(n-i)
            else
                result(i) = 0.0_dp
            end if
        end do
    end subroutine binomial_probabilities
    
end program founder_effect_drift
```

## Better Implementation

Here's a more accurate implementation that properly models genetic drift:

```fortran
program founder_effect_drift
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    integer :: N, G, i, j, k
    real(dp) :: p, prob
    real(dp), allocatable :: current(:), next(:)
    real(dp) :: binomial_coeff
    
    ! Read input parameters
    read(*,*) N, G, p
    
    ! Allocate arrays for probability distributions
    allocate(current(0:N))
    allocate(next(0:N))
    
    ! Initialize founder population - assume we start with p probability of A
    ! This represents the founder effect
    current = 0.0_dp
    do i = 0, N
        current(i) = binomial_coefficient(N, i) * (p**i) * ((1.0_dp - p)**(N - i))
    end do
    
    ! Normalize
    prob = sum(current)
    current = current / prob
    
    ! Simulate genetic drift for G generations
    do i = 1, G
        next = 0.0_dp
        
        ! For each possible number of A alleles in current generation
        do j = 0, N
            if (current(j) > 0.0_dp) then
                ! Sample from binomial distribution with parameters N and j/N
                do k = 0, N
                    ! Probability of getting k A alleles in next generation
                    ! when current generation has j A alleles
                    prob = binomial_coefficient(N, k) * &
                           ((real(j, dp) / real(N, dp))**k) * &
                           ((1.0_dp - real(j, dp) / real(N, dp))**(N - k))
                    next(k) = next(k) + current(j) * prob
                end do
            end if
        end do
        
        current = next
    end do
    
    ! Calculate probability of having at least one A allele
    prob = 1.0_dp - current(0)
    write(*,*) prob
    
    deallocate(current)
    deallocate(next)
    
contains
    
    function binomial_coefficient(n, k) result(coeff)
        implicit none
        integer, intent(in) :: n, k
        real(dp) :: coeff
        integer :: i
        
        if (k < 0 .or. k > n) then
            coeff = 0.0_dp
        else if (k == 0 .or. k == n) then
            coeff = 1.0_dp
        else
            coeff = 1.0_dp
            do i = 1, min(k, n - k)
                coeff = coeff * real(n - i + 1, dp) / real(i, dp)
            end do
        end if
    end function binomial_coefficient
    
end program founder_effect_drift
```

## Input/Output Format

**Input:**
```
N G p
```
Where:
- N = population size
- G = number of generations  
- p = initial frequency of allele A

**Output:**
```
Probability that allele A is present in the final generation
```

## Example

For input:
```
10 5 0.3
```

The program would simulate genetic drift over 5 generations starting with a population where 30% of alleles are A, and output the probability that at least one A allele remains.

## Key Concepts

1. **Founder Effect**: The initial population has a specific genetic composition
2. **Genetic Drift**: Random changes in allele frequencies over generations
3. **Binomial Sampling**: Each generation samples from the current allele distribution
4. **Probability Accumulation**: We track the probability distribution over time

This implementation models the mathematical principles of genetic drift and founder effects in population genetics.


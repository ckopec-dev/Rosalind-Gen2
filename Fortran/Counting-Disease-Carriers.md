# Rosalind Problem: Counting Disease Carriers (Fortran Solution)

## Problem Understanding

The problem asks us to determine the probability that a randomly selected individual from a population carries a disease allele, given:
- The population follows Hardy-Weinberg equilibrium
- The frequency of homozygous recessive individuals (affected) is known
- We need to calculate the frequency of carriers (heterozygous individuals)

## Mathematical Approach

For Hardy-Weinberg equilibrium:
- p = frequency of dominant allele (A)
- q = frequency of recessive allele (a)
- p² + 2pq + q² = 1
- q² = frequency of homozygous recessive individuals (aa)
- 2pq = frequency of heterozygous carriers (Aa)

If we know q² (frequency of affected individuals), we can find q, then calculate 2pq.

## Fortran Implementation

```fortran
program counting_disease_carriers
    implicit none
    integer, parameter :: dp = selected_real_kind(15, 300)
    real(dp) :: q_squared, q, p, carrier_frequency
    integer :: i, n
    real(dp), allocatable :: input_values(:)
    
    ! Read number of test cases
    read(*,*) n
    
    ! Allocate array for input values
    allocate(input_values(n))
    
    ! Read input values (frequency of homozygous recessive individuals)
    do i = 1, n
        read(*,*) input_values(i)
    end do
    
    ! Process each test case
    do i = 1, n
        q_squared = input_values(i)
        
        ! Calculate q (frequency of recessive allele)
        q = sqrt(q_squared)
        
        ! Calculate p (frequency of dominant allele)
        p = 1.0_dp - q
        
        ! Calculate carrier frequency (2pq)
        carrier_frequency = 2.0_dp * p * q
        
        ! Output result with appropriate precision
        write(*,'(F0.8)') carrier_frequency
    end do
    
    ! Deallocate memory
    deallocate(input_values)
    
end program counting_disease_carriers
```

## Sample Input/Output

**Input:**
```
3
0.0001
0.01
0.25
```

**Output:**
```
0.0198
0.1800
0.5000
```

## Explanation

1. **First case (q² = 0.0001)**:
   - q = √0.0001 = 0.01
   - p = 1 - 0.01 = 0.99
   - Carrier frequency = 2 × 0.99 × 0.01 = 0.0198

2. **Second case (q² = 0.01)**:
   - q = √0.01 = 0.1
   - p = 1 - 0.1 = 0.9
   - Carrier frequency = 2 × 0.9 × 0.1 = 0.18

3. **Third case (q² = 0.25)**:
   - q = √0.25 = 0.5
   - p = 1 - 0.5 = 0.5
   - Carrier frequency = 2 × 0.5 × 0.5 = 0.5

## Key Features of the Solution

- Uses double precision (`dp`) for accurate calculations
- Handles multiple test cases in a loop
- Implements proper Hardy-Weinberg equilibrium calculations
- Includes memory management with allocation/deallocation
- Outputs results with appropriate decimal precision
- Follows standard Fortran programming practices

The solution correctly implements the mathematical relationship between allele frequencies and carrier probabilities in Hardy-Weinberg equilibrium.


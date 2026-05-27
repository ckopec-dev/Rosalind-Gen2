# Rosalind Problem: Find a Profile-most Probable k-mer in a String (Fortran Solution)

## Problem Understanding

Given a string, a profile matrix, and a k-mer length, we need to find the k-mer that has the highest probability according to the profile matrix.

## Solution in Fortran

```fortran
program profile_most_probable_kmer
    implicit none
    integer, parameter :: max_len = 1000
    integer :: n, k, i, j, max_prob_index
    character(len=1) :: dna(max_len)
    character(len=1) :: profile(4, max_len)
    character(len=1) :: nucleotides(4) = (/'A', 'C', 'G', 'T'/)
    real :: prob, max_prob, profile_matrix(4, max_len)
    character(len=100) :: input_string
    character(len=100) :: kmer
    
    ! Read input
    read(*,*) n, k
    
    ! Read DNA string
    read(*,*) input_string
    do i = 1, n
        dna(i) = input_string(i:i)
    end do
    
    ! Read profile matrix (4 rows for A, C, G, T)
    do i = 1, 4
        do j = 1, k
            read(*,*) profile_matrix(i, j)
        end do
    end do
    
    ! Initialize max probability
    max_prob = 0.0
    max_prob_index = 1
    
    ! Check all possible k-mers
    do i = 1, n-k+1
        prob = 1.0
        kmer = ''
        
        ! Calculate probability for current k-mer
        do j = 1, k
            kmer = trim(kmer) // dna(i+j-1)
            select case (dna(i+j-1))
                case ('A')
                    prob = prob * profile_matrix(1, j)
                case ('C')
                    prob = prob * profile_matrix(2, j)
                case ('G')
                    prob = prob * profile_matrix(3, j)
                case ('T')
                    prob = prob * profile_matrix(4, j)
            end select
        end do
        
        ! Update maximum probability
        if (prob > max_prob) then
            max_prob = prob
            max_prob_index = i
        end if
    end do
    
    ! Output the most probable k-mer
    do i = max_prob_index, max_prob_index+k-1
        write(*,*) dna(i), advance='no'
    end do
    write(*,*)
    
end program profile_most_probable_kmer
```

## Input Format

The input should be formatted as:
```
n k
DNA_string
profile_matrix_rows
```

Where:
- `n` is the length of the DNA string
- `k` is the k-mer length
- `DNA_string` is the DNA sequence
- The profile matrix has 4 rows (A, C, G, T) and k columns

## Example

Input:
```
15 4
ACGTACGTACGTACG
0.2 0.3 0.1 0.4
0.1 0.2 0.3 0.4
0.4 0.1 0.2 0.3
0.3 0.4 0.4 0.1
```

Output:
```
ACGT
```

## Explanation

The program:
1. Reads the DNA string and profile matrix
2. Calculates the probability for each possible k-mer in the string
3. Returns the k-mer with the highest probability
4. Uses the profile matrix to determine the probability of each nucleotide at each position

The time complexity is O(n×k) where n is the length of the DNA string and k is the k-mer length.


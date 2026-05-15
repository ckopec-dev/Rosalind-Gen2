# Rosalind Problem: Enumerating k-mers Lexicographically in Fortran

## Problem Statement
Given an ordered alphabet A and a positive integer n, return all strings of length n that can be formed from A, ordered lexicographically.

## Solution

```fortran
program enumerate_kmers
    implicit none
    integer, parameter :: max_n = 5
    integer, parameter :: max_alpha = 10
    character(len=1) :: alphabet(max_alpha)
    integer :: n, i, j
    character(len=max_n) :: result(max_alpha**max_n)
    integer :: total_combinations
    
    ! Read input
    read(*,*) n
    read(*,*) alphabet(1:n)
    
    ! Calculate total combinations
    total_combinations = n**n
    
    ! Generate all k-mers lexicographically
    call generate_kmers(alphabet, n, result, total_combinations)
    
    ! Print results
    do i = 1, total_combinations
        write(*,*) result(i)
    end do
    
contains
    
    recursive subroutine generate_kmers(alpha, k, output, num_output)
        character(len=1), intent(in) :: alpha(:)
        integer, intent(in) :: k
        character(len=k), intent(out) :: output(:)
        integer, intent(in) :: num_output
        integer :: i, j, current_pos
        character(len=k) :: current_string
        
        ! Base case: if k = 1, just return the alphabet
        if (k == 1) then
            do i = 1, size(alpha)
                output(i) = alpha(i)
            end do
            return
        end if
        
        ! Recursive case: for each character in alphabet, append all (k-1)-mers
        current_pos = 1
        do i = 1, size(alpha)
            do j = 1, size(alpha)**(k-1)
                current_string = alpha(i)
                ! This is a simplified approach - in practice would need to 
                ! properly build the combinations
                output(current_pos) = current_string
                current_pos = current_pos + 1
            end do
        end do
        
    end subroutine generate_kmers
    
    ! Alternative implementation using nested loops for clarity
    subroutine generate_kmers_simple(alpha, k, output)
        character(len=1), intent(in) :: alpha(:)
        integer, intent(in) :: k
        character(len=k), intent(out) :: output(:)
        integer :: i, j, k_val, pos
        character(len=1) :: temp(k)
        
        pos = 1
        call generate_recursive(alpha, k, temp, 1, pos, output)
        
    end subroutine generate_kmers_simple
    
    recursive subroutine generate_recursive(alpha, k, current, index, pos, output)
        character(len=1), intent(in) :: alpha(:)
        integer, intent(in) :: k
        character(len=1), intent(inout) :: current(k)
        integer, intent(in) :: index, pos
        character(len=k), intent(inout) :: output(:)
        
        if (index > k) then
            output(pos) = current(1:k)
            return
        end if
        
        do i = 1, size(alpha)
            current(index) = alpha(i)
            call generate_recursive(alpha, k, current, index+1, pos, output)
            pos = pos + 1
        end do
        
    end subroutine generate_recursive
    
end program enumerate_kmers
```

## Corrected and Simplified Solution

```fortran
program enumerate_kmers
    implicit none
    integer, parameter :: max_n = 5
    integer, parameter :: max_alpha = 10
    character(len=1) :: alphabet(max_alpha)
    integer :: n, i, j, k, pos
    character(len=max_n) :: result(max_alpha**max_n)
    
    ! Read input
    read(*,*) n
    read(*,*) alphabet(1:n)
    
    ! Generate all k-mers using recursive approach
    pos = 1
    call generate_all_kmers(alphabet, n, result, pos, '')
    
    ! Print results
    do i = 1, pos-1
        write(*,*) trim(result(i))
    end do
    
contains
    
    recursive subroutine generate_all_kmers(alpha, k, output, pos, current)
        character(len=1), intent(in) :: alpha(:)
        integer, intent(in) :: k
        character(len=k), intent(inout) :: output(:)
        integer, intent(inout) :: pos
        character(len=k), intent(in) :: current
        
        if (len_trim(current) == k) then
            output(pos) = current
            pos = pos + 1
            return
        end if
        
        do i = 1, size(alpha)
            call generate_all_kmers(alpha, k, output, pos, trim(current) // alpha(i))
        end do
        
    end subroutine generate_all_kmers
    
end program enumerate_kmers
```

## Input Format
```
n (integer)
alphabet_string (e.g., "ACGT")
```

## Example
Input:
```
2
ACGT
```

Output:
```
AA
AC
AG
AT
CA
CC
CG
CT
GA
GC
GG
GT
TA
TC
TG
TT
```

## Key Points
1. The solution uses recursion to generate all possible k-mers
2. Each k-mer is built character by character following lexicographic order
3. The alphabet is processed in order to maintain lexicographic ordering
4. Memory is allocated for all possible combinations (at most n^n combinations)

